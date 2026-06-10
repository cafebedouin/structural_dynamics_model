% ============================================================================
% CONSTRAINT STORY: property_tax_entitlement
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-01-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_property_tax_entitlement, []).

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
 *   constraint_id: property_tax_entitlement
 *   human_readable: Property Tax Entitlement to Curb Space
 *   domain: urban_planning/public_resource_allocation/transportation_policy
 *
 * SUMMARY:
 *   The property-tax entitlement reading of curb space governance asserts
 *   that homeowners who pay property taxes have purchased access to unlimited
 *   free residential parking on public streets adjacent to their property.
 *   This reading is one of three structurally distinct interpretations of the
 *   same curb space legitimacy kernel. Under this reading, curb space is
 *   treated as a quasi-private good bundled with homeownership: property
 *   taxes fund municipal services including parking provision, and homeowners
 *   are entitled to park on 'their' block as a return on their tax payment.
 *   Most U.S. municipalities implement this reading through zero-fee or
 *   nominal-fee residential parking permits with no household vehicle limits.
 *   The arrangement solves a genuine coordination problem (preventing parking
 *   chaos in residential neighborhoods) but embeds substantial extraction:
 *   non-car-owners pay property taxes but receive zero parking benefit;
 *   renters pay indirectly through rent but face higher permit fees where
 *   they exist; multi-vehicle households capture disproportionate value. The
 *   constraint has intensified over the 30-year interval as vehicle ownership
 *   per household has increased, curb space has become more contested, and
 *   the opportunity cost of dedicating curb space to free parking (versus bus
 *   lanes, bike lanes, or paid parking) has risen.
 *
 * KEY AGENTS:
 *   - Multi-Vehicle Homeowners: Primary beneficiary (institutional/arbitrage) — unlimited free parking for multiple vehicles; property tax payment unlocks access with no marginal cost per vehicle
 *   - Non-Car-Owning Residents: Primary victim (powerless/trapped) — pay property taxes (directly or through rent) but receive zero parking benefit; cannot exit tax obligation or access the resource
 *   - Single-Car Renters: Secondary victim (moderate/constrained) — benefit from parking access but at higher effective cost than homeowners; constrained by lease and vehicle dependency
 *   - Municipal Transportation Department: Institutional actor (institutional/constrained) — manages curb allocation but politically captured by homeowner coalitions; experiences both coordination function and extractive constraint
 *   - Parking Reform Coalition: Organized agents (organized/mobile) — advocacy groups building alternative governance models with sunset logic (congestion pricing, permit fees, parking maximums)
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees genuine coordination function embedded with substantial extraction; recognizes property-tax bundling as contingent institutional choice rather than natural law
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(property_tax_entitlement, 0.58).
domain_priors:suppression_score(property_tax_entitlement, 0.67).
domain_priors:theater_ratio(property_tax_entitlement, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(property_tax_entitlement, extractiveness, 0.58).
narrative_ontology:constraint_metric(property_tax_entitlement, suppression_requirement, 0.67).
narrative_ontology:constraint_metric(property_tax_entitlement, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(property_tax_entitlement, accessibility_collapse, 0.52).
narrative_ontology:constraint_metric(property_tax_entitlement, resistance, 0.64).

% --- Constraint claim ---
narrative_ontology:constraint_claim(property_tax_entitlement, tangled_rope).
narrative_ontology:human_readable(property_tax_entitlement, "Property Tax Entitlement to Curb Space").
narrative_ontology:topic_domain(property_tax_entitlement, "urban_planning/public_resource_allocation/transportation_policy").

domain_priors:requires_active_enforcement(property_tax_entitlement).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(property_tax_entitlement, 'adf8f30c-4ec5-492a-aea7-7da7005f4bff').
narrative_ontology:cs_kernel_codification('adf8f30c-4ec5-492a-aea7-7da7005f4bff', distributed).
narrative_ontology:cs_authority_grounding('adf8f30c-4ec5-492a-aea7-7da7005f4bff', practice).
narrative_ontology:cs_interpretation_layer_present('adf8f30c-4ec5-492a-aea7-7da7005f4bff').
narrative_ontology:cs_reading_relation('adf8f30c-4ec5-492a-aea7-7da7005f4bff', property_tax_entitlement__public_resource_pricing, coexists_with).
narrative_ontology:cs_reading_relation('adf8f30c-4ec5-492a-aea7-7da7005f4bff', property_tax_entitlement__equity_redistribution, coexists_with).
narrative_ontology:cs_axiom('adf8f30c-4ec5-492a-aea7-7da7005f4bff', foundational, property_tax_purchases_public_goods_access).
narrative_ontology:cs_axiom_status(property_tax_purchases_public_goods_access, holdable).
narrative_ontology:cs_axiom_grounding('adf8f30c-4ec5-492a-aea7-7da7005f4bff', property_tax_purchases_public_goods_access, conventional).
narrative_ontology:cs_axiom('adf8f30c-4ec5-492a-aea7-7da7005f4bff', secondary, homeowner_priority_over_public_right_of_way).
narrative_ontology:cs_axiom_status(homeowner_priority_over_public_right_of_way, holdable).
narrative_ontology:cs_axiom_grounding('adf8f30c-4ec5-492a-aea7-7da7005f4bff', homeowner_priority_over_public_right_of_way, conventional).
narrative_ontology:cs_reference_frame('adf8f30c-4ec5-492a-aea7-7da7005f4bff', mid_century_automobile_expansion).
narrative_ontology:cs_drift_state('adf8f30c-4ec5-492a-aea7-7da7005f4bff', contemporary_urban_density, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('adf8f30c-4ec5-492a-aea7-7da7005f4bff', '').
narrative_ontology:cs_kernel_id(property_tax_entitlement, curb_space_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(property_tax_entitlement, homeowners_with_vehicles).
narrative_ontology:constraint_beneficiary(property_tax_entitlement, multi_vehicle_households).
narrative_ontology:constraint_victim(property_tax_entitlement, non_car_owning_residents).
narrative_ontology:constraint_victim(property_tax_entitlement, renters_with_vehicles).
narrative_ontology:constraint_victim(property_tax_entitlement, visitors_and_commercial_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(property_tax_entitlement, multi_vehicle_homeowners).
narrative_ontology:constraint_beneficiary(property_tax_entitlement, single_car_renters).
narrative_ontology:constraint_victim(property_tax_entitlement, single_car_renters).
narrative_ontology:constraint_vindicates(property_tax_entitlement, property_tax_purchases_public_goods_access).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Homeowners with multiple vehicles who pay property taxes and receive unlimited free residential parking permits. Can store 2-4 vehicles on public curb space at zero marginal cost per vehicle. Property tax payment (typically $3,000-$8,000/year depending on jurisdiction) unlocks parking access worth $1,200-$3,600 per vehicle per year if valued at market rates. Exit options include relocating to jurisdictions with more favorable parking policy or selling vehicles, but neither is necessary — the arrangement is net-positive from their position.
narrative_ontology:constraint_stakeholder(property_tax_entitlement, multi_vehicle_homeowners, beneficiary,
    institutional, immediate, arbitrage, local).

% Residents who do not own vehicles but pay property taxes (directly as homeowners or indirectly through rent). Receive zero benefit from curb space allocation despite funding it through taxes. Cannot exit the tax obligation without leaving the jurisdiction entirely. In dense urban neighborhoods, non-car-owners may constitute 20-40% of residents but receive 0% of the parking subsidy. The arrangement extracts from them to subsidize vehicle-owning neighbors.
narrative_ontology:constraint_stakeholder(property_tax_entitlement, non_car_owning_residents, payer,
    powerless, biographical, trapped, local).

% Renters with one vehicle who pay property taxes indirectly through rent and may face additional permit fees ($25-$150/year depending on jurisdiction). Benefit from parking access but at higher effective cost than homeowners. Constrained by residential lease (typically 1-year term) and vehicle dependency (need car for work commute or family obligations). Experience both coordination (parking access enables car ownership) and extraction (unequal cost structure relative to homeowners, plus indirect subsidy of multi-vehicle households).
narrative_ontology:constraint_stakeholder(property_tax_entitlement, single_car_renters, payer,
    moderate, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(property_tax_entitlement, single_car_renters, beneficiary).

% City agency responsible for curb space management, permit issuance, and parking enforcement. Administers the property-tax entitlement system but is politically captured by homeowner coalitions who demand free parking and block reallocation proposals. Benefits from administrative authority and enforcement revenue but constrained by political economy. Proposals to increase permit fees, limit permits per household, or reallocate curb space to bus lanes face organized homeowner opposition and are typically defeated or watered down.
narrative_ontology:constraint_stakeholder(property_tax_entitlement, municipal_transportation_department, agenda_setter,
    institutional, biographical, constrained, local).

% Organized advocacy groups (Parking Reform Network, Strong Towns, YIMBY coalitions, transportation equity organizations) working to dismantle the property-tax entitlement model. Promote alternative governance models including congestion pricing, residential permit fees scaled to vehicle count, parking maximums replacing minimums, and unbundling parking from housing costs. Have achieved policy wins in reform-oriented cities (San Francisco, Seattle, Minneapolis) but face entrenched opposition in most jurisdictions. See the entitlement model as a transitional arrangement with a 15-25 year sunset as norms shift.
narrative_ontology:constraint_stakeholder(property_tax_entitlement, parking_reform_coalition, observer,
    organized, generational, mobile, national).

% Non-residents who need curb access for visiting, deliveries, or commercial purposes. Excluded from residential permit zones and must pay metered parking rates ($2-$5/hour) or risk tickets. Mobile in the sense that they can choose not to visit permit-restricted neighborhoods, but this exit option imposes costs on both visitors and residents (reduced commerce, reduced social connection). The arrangement prioritizes residential vehicle storage over visitor access and commercial activity.
narrative_ontology:constraint_stakeholder(property_tax_entitlement, visitors_and_commercial_users, excluded,
    powerless, immediate, mobile, regional).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(property_tax_entitlement, multi_vehicle_homeowners).
narrative_ontology:fixing_cost_class(property_tax_entitlement, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Prevents parking chaos in residential neighborhoods by allocating curb space to residents rather than allowing unrestricted competition for spots. Solves the genuine problem of ensuring residents can park near their homes without circling for hours or parking miles away.
% TRANSFER_FUNCTION: Transfers curb space access (a scarce public resource) from the general public to vehicle-owning homeowners at zero or nominal cost. Transfers wealth from non-car-owners and renters (who pay property taxes but receive no parking benefit) to multi-vehicle homeowners (who capture disproportionate value). Estimated annual subsidy: $1,200-$3,600 per vehicle if curb space were priced at market rates.
% ABSENT_VOICES: Non-car-owning residents are systematically excluded from parking policy debates because they are not organized and have no direct stake in permit rules. Renters are underrepresented because they are more transient and less likely to attend public hearings. Future residents who would prefer transit-oriented development with less parking are absent because they have not yet moved to the neighborhood. The arrangement is designed and defended by current vehicle-owning homeowners; dissenting voices are not in the room.
% DISAPPEARANCE_RATIONALE: If the property-tax entitlement disappeared overnight, parking allocation would need to be reorganized through alternative mechanisms (metered parking, market-rate permits, time limits, or first-come-first-served). Homeowners would lose their unlimited free parking access and would need to pay market rates or reduce vehicle ownership. Non-car-owners would no longer subsidize vehicle owners through property taxes. Curb space could be reallocated to alternative uses (bus lanes, bike lanes, parklets, loading zones) that serve more people per square foot. The world would rearrange itself around a different governance model.
% FOUNDING_PROBLEM: The property-tax entitlement model emerged in the mid-20th century as automobile ownership expanded and residential streets became congested. The founding problem was preventing parking chaos in neighborhoods where most households owned one car and curb space was relatively abundant. Property taxes were already funding street maintenance and traffic management; bundling parking access onto the existing tax base was administratively simple and politically popular with homeowners.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (parking chaos prevention) is corroborated by transportation historians and urban planners as a genuine mid-20th-century coordination challenge. However, the status of the problem is contested: parking reform advocates argue that the problem has changed (curb space is now scarce, vehicle ownership per household has increased, alternative transportation modes are viable) and the property-tax entitlement is no longer the appropriate solution. Homeowner coalitions argue the problem is still live and the entitlement should be preserved. Municipal transportation departments are split — some acknowledge the problem has evolved, others defend the status quo. No neutral outside observer confirms that unlimited free parking for homeowners is still the optimal solution to the coordination problem.
narrative_ontology:disappearance_verdict(property_tax_entitlement, world_rearranges).
narrative_ontology:founding_problem_status(property_tax_entitlement, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: NON-CAR-OWNING RESIDENT (SNARE) — Pays property taxes (directly as homeowner or indirectly through rent) but receives zero benefit from curb allocation. Cannot exit the tax obligation; cannot access the resource their taxes fund. Experiences pure extraction with no coordination benefit.
constraint_indexing:constraint_classification(property_tax_entitlement, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SINGLE-CAR RENTER (TANGLED ROPE) — Benefits from curb access but at higher effective cost than homeowners (permit fees where they exist, plus indirect property tax through rent). Constrained by residential lease and vehicle dependency. Experiences both coordination (parking access enables car ownership) and extraction (unequal cost structure relative to homeowners).
constraint_indexing:constraint_classification(property_tax_entitlement, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MULTI-VEHICLE HOMEOWNER (ROPE) — Primary beneficiary. Property tax payment unlocks unlimited residential parking permits (or zero-fee permits in most jurisdictions). Can store multiple vehicles on public right-of-way at negligible marginal cost. Experiences pure coordination: tax payment solves the parking problem with no meaningful extraction.
constraint_indexing:constraint_classification(property_tax_entitlement, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: PARKING REFORM COALITION (SCAFFOLD) — Organized advocacy groups (Parking Reform Network, Strong Towns, YIMBY coalitions) see the property-tax entitlement as a transitional arrangement being actively dismantled. Sunset mechanisms include: congestion pricing adoption, residential permit fee increases, parking maximums replacing minimums, and unbundling parking from housing costs. Estimated sunset: 15-25 years for norm shift in major metros.
constraint_indexing:constraint_classification(property_tax_entitlement, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: MUNICIPAL TRANSPORTATION DEPARTMENT (TANGLED ROPE) — Institutional actor caught between coordination function (managing curb access to prevent chaos) and extraction pressure (political capture by homeowner coalitions demanding free parking). Benefits from administrative authority but constrained by political economy. Experiences the arrangement as both necessary coordination and extractive political constraint.
constraint_indexing:constraint_classification(property_tax_entitlement, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(local))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, the constraint solves a genuine coordination problem (allocating scarce curb space) but embeds substantial extraction (regressive subsidy to vehicle owners, exclusion of non-drivers from benefit pool, opportunity cost of alternative curb uses). The property-tax bundling is a contingent institutional choice, not a natural law, but it does provide coordination function alongside its extractive elements.
constraint_indexing:constraint_classification(property_tax_entitlement, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(property_tax_entitlement_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(property_tax_entitlement, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(property_tax_entitlement, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(property_tax_entitlement, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(property_tax_entitlement_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The arrangement extracts from non-drivers (who pay taxes but receive no benefit) and from renters (who pay indirectly but face higher permit costs). Multi-vehicle households capture disproportionate value — a household with three cars receives 3x the curb space subsidy of a one-car household despite paying the same property tax. The extraction has increased over the interval as vehicle ownership per household has risen and curb space has become more contested. However, extraction is not maximal because the arrangement does solve a coordination problem and some beneficiaries (single-car homeowners) receive roughly proportional benefit. Suppression (0.67): Moderate-high. Non-drivers cannot exit the property tax obligation and cannot access the parking benefit. Renters are locked into the arrangement through their lease. Alternative curb uses (bus lanes, bike lanes, parklets) are suppressed by political capture — homeowner coalitions block reallocation proposals. Suppression has increased over the interval as enforcement has intensified (more aggressive ticketing of non-permit holders, expansion of permit zones). Theater ratio (0.41): Moderate. Some performative elements exist (permit application processes that serve no verification function, public hearings on parking policy that are captured by homeowner coalitions) but the arrangement is not primarily theatrical — it does allocate curb space, even if inequitably. Theater has increased as the coordination function has been displaced by rent-seeking (permit systems that exist to exclude rather than to manage scarcity). Accessibility collapse (0.52): Moderate. Alternative governance models exist and are being implemented in some jurisdictions (San Francisco's congestion pricing, Seattle's paid parking expansion, Minneapolis's parking maximum ordinance). The property-tax entitlement is not a natural law — it is a contingent institutional choice. However, alternatives face significant political barriers and the entitlement reading remains dominant in most U.S. municipalities. Resistance (0.64): Moderate-high. The arrangement faces substantial organized resistance from parking reform coalitions, urbanist advocacy groups, and equity-focused transportation planners. However, homeowner coalitions defend the entitlement vigorously and have significant political power.
 *
 * PERSPECTIVAL GAP:
 *   The property-tax entitlement reading produces a wide perspectival gap. Multi-vehicle homeowners experience pure coordination (Rope) — their property tax payment solves the parking problem with no meaningful cost. Non-car-owning residents experience pure extraction (Snare) — they pay taxes but receive zero benefit and cannot exit. Single-car renters and the municipal transportation department experience mixed coordination and extraction (Tangled Rope) — the arrangement both enables and constrains them. The parking reform coalition sees a temporary problem with a sunset (Scaffold) — alternative governance models are being built and will eventually replace the entitlement reading. The analytical observer sees the arrangement as genuinely hybrid (Tangled Rope) — it solves a coordination problem but embeds substantial extraction. The gap reveals that the 'property taxes purchase parking access' framing is not a natural law but a contested institutional choice that benefits vehicle-owning homeowners at the expense of non-drivers and renters.
 *
 * DIRECTIONALITY LOGIC:
 *   Multi-vehicle homeowners are full beneficiaries with arbitrage exit options — they capture maximum value from the arrangement and can relocate to jurisdictions with more favorable parking policy if needed. Their directionality is near 0.0 (full beneficiary), producing negative or near-zero effective extraction. Non-car-owning residents are full victims with trapped exit options — they pay property taxes but receive zero parking benefit and cannot exit the tax obligation. Their directionality is near 1.0 (full target), producing maximum effective extraction. Single-car renters are partial victims with constrained exit options — they benefit from parking access but at higher cost than homeowners and are locked in by lease and vehicle dependency. Their directionality is moderate (~0.6), producing moderate effective extraction. The municipal transportation department is an institutional actor with constrained exit options — it benefits from administrative authority but is politically captured by homeowner coalitions. Its directionality is moderate (~0.5), reflecting mixed coordination and extraction. The parking reform coalition has mobile exit options and sees the arrangement as temporary — its directionality is low (~0.3), producing low effective extraction. The analytical observer has analytical exit options and sees both coordination and extraction — its directionality is moderate (~0.5).
 *
 * MANDATROPHY ANALYSIS:
 *   The property-tax entitlement reading resolves the mandatrophy by demonstrating that curb space governance is a contested kernel with multiple structurally distinct readings, not a single natural coordination mechanism. The entitlement reading's coordination function (preventing parking chaos) is genuine but does not require the specific institutional form of unlimited free parking for homeowners. Alternative readings (public resource pricing, equity redistribution) solve the same coordination problem with different extraction profiles. The mandatrophy is not 'which reading is correct?' but 'which reading does this jurisdiction implement, and who benefits from that choice?' The analytical observer's tangled-rope classification captures the hybrid nature: genuine coordination embedded with substantial extraction. The constraint's intensification over the 30-year interval (rising extractiveness, rising suppression) reflects increasing vehicle ownership and curb space scarcity, not a change in the underlying coordination problem.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    property_tax_benefit_theory,
    'Does property tax payment legitimately entitle residents to curb space access, or is this a post-hoc rationalization of an extractive subsidy?',
    'Historical analysis of property tax adoption vs parking policy adoption; cross-jurisdictional comparison of property tax rates and parking provision; examination of whether property taxes were explicitly raised to fund parking infrastructure or whether parking access was bundled onto existing tax base.',
    'If property taxes were raised to fund parking: coordination function is genuine and extraction is lower. If parking access was bundled onto existing tax base without explicit voter authorization: extraction is higher and the benefit-theory framing is cover.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(property_tax_benefit_theory, empirical, 'Whether property tax payment legitimately purchases parking access').

omega_variable(
    alternative_curb_use_valuation,
    'What is the opportunity cost of dedicating curb space to free residential parking versus alternative uses (bike lanes, bus lanes, parklets, loading zones, paid parking)?',
    'Economic valuation studies of curb space; transportation mode shift analysis when curb space is reallocated; revenue analysis from paid parking vs free residential permits.',
    'If opportunity cost is low: extraction is lower and coordination function dominates. If opportunity cost is high (e.g., curb space for bus lanes would serve 10x more people): extraction is higher and the arrangement is misallocation rather than coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(alternative_curb_use_valuation, empirical, 'Opportunity cost of free residential parking vs alternative curb uses').

omega_variable(
    kernel_reading_ambiguity,
    'Is the property-tax entitlement reading the legitimate interpretation of curb space governance, or is it one contested reading among several structurally distinct alternatives?',
    'Cross-jurisdictional analysis of curb space governance models; examination of whether the property-tax bundling is universal or varies by municipality; identification of alternative readings (public resource pricing, equity redistribution) and their institutional support.',
    'If property-tax entitlement is universal: it may be a natural coordination mechanism. If it is one reading among contested alternatives: it is a contingent institutional choice that benefits specific actors (homeowners with vehicles) at the expense of others (non-drivers, renters).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether property-tax entitlement is the only coherent reading of curb space legitimacy').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(property_tax_entitlement, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(curb_prop_tr_t0, property_tax_entitlement, theater_ratio, 0, 0.28).
narrative_ontology:measurement(curb_prop_tr_t10, property_tax_entitlement, theater_ratio, 10, 0.33).
narrative_ontology:measurement(curb_prop_tr_t20, property_tax_entitlement, theater_ratio, 20, 0.38).
narrative_ontology:measurement(curb_prop_tr_t30, property_tax_entitlement, theater_ratio, 30, 0.41).

% Extraction over time
narrative_ontology:measurement(curb_prop_be_t0, property_tax_entitlement, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(curb_prop_be_t10, property_tax_entitlement, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(curb_prop_be_t20, property_tax_entitlement, base_extractiveness, 20, 0.54).
narrative_ontology:measurement(curb_prop_be_t30, property_tax_entitlement, base_extractiveness, 30, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(curb_prop_su_t0, property_tax_entitlement, suppression_requirement, 0, 0.52).
narrative_ontology:measurement(curb_prop_su_t10, property_tax_entitlement, suppression_requirement, 10, 0.58).
narrative_ontology:measurement(curb_prop_su_t20, property_tax_entitlement, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(curb_prop_su_t30, property_tax_entitlement, suppression_requirement, 30, 0.67).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(property_tax_entitlement, resource_allocation).
narrative_ontology:affects_constraint(property_tax_entitlement, residential_parking_minimum_requirements).
narrative_ontology:affects_constraint(property_tax_entitlement, street_design_automobile_priority).

% DUAL FORMULATION NOTE:
% The property-tax entitlement reading is one of three readings of the curb_space_legitimacy kernel. The other readings (public_resource_pricing, equity_redistribution) are separate constraint stories with different beneficiary/victim structures and different extractiveness values. This story models only the property_tax_entitlement reading; the sibling readings are linked via the kernel structure but are not part of this constraint's classification.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
