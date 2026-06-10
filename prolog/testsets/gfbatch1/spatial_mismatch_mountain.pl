% ============================================================================
% CONSTRAINT STORY: spatial_mismatch_mountain
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2025-06-10
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_spatial_mismatch_mountain, []).

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
    domain_priors:emerges_naturally/1,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   constraint_id: spatial_mismatch_mountain
 *   human_readable: Spatial Mismatch: Fixed Curb Length vs. Growing Vehicle Fleet
 *   domain: urban_planning/transportation_policy/public_resource_allocation
 *
 * SUMMARY:
 *   The spatial mismatch between fixed urban curb length and growing vehicle
 *   fleet is presented as a natural law of urbanization — a mountain that no
 *   policy can eliminate. The physical constraint is genuine: street geometry
 *   limits curb-miles, and vehicle ownership grows with population and
 *   income. However, the constraint's classification depends critically on
 *   the observer's structural position and the decomposition of physical
 *   versus policy-induced scarcity. Early vehicle adopters experience the
 *   constraint as coordination (they claimed space when it was abundant).
 *   Late adopters and non-car-owners experience it as extraction (they bear
 *   the scarcity cost while early adopters retain access). The analytical
 *   observer must distinguish the immutable physical constraint (fixed curb
 *   length) from the contingent allocation rule (free or underpriced permits)
 *   that determines who benefits and who pays. The beneficiary structure —
 *   early adopters and high-car-ownership households — suggests that the
 *   'natural' scarcity may be a false summit: a physical constraint amplified
 *   and naturalized by a policy choice that protects incumbents.
 *
 * KEY AGENTS:
 *   - Daily Circler: Primary victim (powerless/trapped) — experiences the constraint as unchangeable; bears circling time and search costs with no exit
 *   - Suburban Commuter: Secondary victim (moderate/constrained) — could relocate or change jobs at high cost; sees the scarcity as a fixed feature of urban density
 *   - Early Vehicle Adopter: Primary beneficiary (institutional/arbitrage) — acquired vehicles when curb space was abundant; now benefits from first-come allocation lock-in
 *   - High-Car-Ownership Households: Secondary beneficiary (institutional/arbitrage) — multiple vehicles per household capture disproportionate curb space under free or underpriced allocation
 *   - Transportation Planner Coalition: Organized agents (organized/mobile) — see both physical constraint and policy constraint; advocate for pricing reforms but face political resistance
 *   - Analytical Observer: Civilizational view (analytical/analytical) — must distinguish genuine physical constraint from false summit (policy-amplified scarcity naturalized as geometry)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(spatial_mismatch_mountain, 0.15).
domain_priors:suppression_score(spatial_mismatch_mountain, 0.2).
domain_priors:theater_ratio(spatial_mismatch_mountain, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(spatial_mismatch_mountain, extractiveness, 0.15).
narrative_ontology:constraint_metric(spatial_mismatch_mountain, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(spatial_mismatch_mountain, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(spatial_mismatch_mountain, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(spatial_mismatch_mountain, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(spatial_mismatch_mountain, mountain).
narrative_ontology:human_readable(spatial_mismatch_mountain, "Spatial Mismatch: Fixed Curb Length vs. Growing Vehicle Fleet").
narrative_ontology:topic_domain(spatial_mismatch_mountain, "urban_planning/transportation_policy/public_resource_allocation").

domain_priors:emerges_naturally(spatial_mismatch_mountain).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(spatial_mismatch_mountain, 'c16fcc10-0cc6-4fe6-aab4-f486b379f3d4').
narrative_ontology:cs_kernel_codification('c16fcc10-0cc6-4fe6-aab4-f486b379f3d4', distributed).
narrative_ontology:cs_authority_grounding('c16fcc10-0cc6-4fe6-aab4-f486b379f3d4', lineage).
narrative_ontology:cs_reading_relation('c16fcc10-0cc6-4fe6-aab4-f486b379f3d4', spatial_mismatch_mountain__curb_space_property_tax_entitlement, influences).
narrative_ontology:cs_reading_relation('c16fcc10-0cc6-4fe6-aab4-f486b379f3d4', spatial_mismatch_mountain__curb_space_public_resource_pricing, coexists_with).
narrative_ontology:cs_reading_relation('c16fcc10-0cc6-4fe6-aab4-f486b379f3d4', spatial_mismatch_mountain__curb_space_equity_redistribution, coexists_with).
narrative_ontology:cs_axiom('c16fcc10-0cc6-4fe6-aab4-f486b379f3d4', foundational, curb_scarcity_is_physical_geometry).
narrative_ontology:cs_axiom_status(curb_scarcity_is_physical_geometry, holdable).
narrative_ontology:cs_axiom_grounding('c16fcc10-0cc6-4fe6-aab4-f486b379f3d4', curb_scarcity_is_physical_geometry, empirically_contingent).
narrative_ontology:cs_axiom('c16fcc10-0cc6-4fe6-aab4-f486b379f3d4', secondary, allocation_rule_is_coordination_not_extraction).
narrative_ontology:cs_axiom_status(allocation_rule_is_coordination_not_extraction, holdable).
narrative_ontology:cs_axiom_grounding('c16fcc10-0cc6-4fe6-aab4-f486b379f3d4', allocation_rule_is_coordination_not_extraction, conventional).
narrative_ontology:cs_reference_frame('c16fcc10-0cc6-4fe6-aab4-f486b379f3d4', property_tax_entitlement_norm).
narrative_ontology:cs_drift_state('c16fcc10-0cc6-4fe6-aab4-f486b379f3d4', contemporary_pricing_reform_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('c16fcc10-0cc6-4fe6-aab4-f486b379f3d4', '2025-06-10T14:32:00Z').

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(spatial_mismatch_mountain, early_vehicle_adopters).
narrative_ontology:constraint_beneficiary(spatial_mismatch_mountain, high_car_ownership_households).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(spatial_mismatch_mountain, early_vehicle_adopter).
narrative_ontology:constraint_beneficiary(spatial_mismatch_mountain, high_car_ownership_household).
narrative_ontology:constraint_victim(spatial_mismatch_mountain, daily_circler).
narrative_ontology:constraint_victim(spatial_mismatch_mountain, non_car_owner).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Circles the block searching for parking; bears time cost and fuel cost with no alternative. Cannot relocate, cannot afford off-street parking, cannot access transit in low-density context. Experiences the scarcity as unchangeable geometry.
narrative_ontology:constraint_stakeholder(spatial_mismatch_mountain, daily_circler, payer,
    powerless, immediate, trapped, local).

% Acquired vehicle when curb space was abundant (1960s-1980s); claimed residential parking space under free or nominal-fee permit system. Now benefits from allocation lock-in: retains access as scarcity intensifies, pays below-market rate (or zero), and faces no competition from late adopters or non-residents. Could sell vehicle and use alternatives, but chooses not to because the subsidy makes car ownership artificially cheap.
narrative_ontology:constraint_stakeholder(spatial_mismatch_mountain, early_vehicle_adopter, beneficiary,
    institutional, biographical, arbitrage, local).

% Owns multiple vehicles (2-4 per household); captures disproportionate curb space under free or underpriced allocation. Each vehicle receives a permit at the same nominal cost, so the household's subsidy scales with vehicle count. Could reduce vehicle ownership or pay for off-street parking, but the curb subsidy makes multiple-car ownership artificially cheap.
narrative_ontology:constraint_stakeholder(spatial_mismatch_mountain, high_car_ownership_household, beneficiary,
    institutional, biographical, arbitrage, local).

% Does not own a vehicle; subsidizes curb space allocation via general property taxes and sales taxes that fund street maintenance. Receives no direct benefit from the curb (cannot park a car they don't own) but pays the opportunity cost: curb space allocated to free parking could be reallocated to bus lanes, bike lanes, or pedestrian space that would benefit non-car-owners. Could acquire a vehicle to access the subsidy, but cannot afford it or chooses not to for environmental or financial reasons.
narrative_ontology:constraint_stakeholder(spatial_mismatch_mountain, non_car_owner, payer,
    powerless, biographical, constrained, local).

% Advocates for curb pricing reforms (market-rate permits, congestion pricing, parking maximums) to manage demand and fund alternative mobility. Sees both the physical constraint (fixed curb length) and the policy constraint (free allocation creates excess demand). Faces political resistance from incumbent parkers (homeowners, business owners) who benefit from the status quo. Can advocate for change but cannot unilaterally implement it.
narrative_ontology:constraint_stakeholder(spatial_mismatch_mountain, transportation_planner, agenda_setter,
    organized, generational, mobile, national).

% Sets curb allocation policy (permit fees, caps, eligibility rules) under political pressure from homeowner constituency. Faces electoral risk from pricing reforms that would anger incumbent parkers. Could implement market-rate pricing but chooses not to because the political cost exceeds the policy benefit (from the legislator's immediate time horizon). Constrained by electoral cycle and homeowner voting bloc.
narrative_ontology:constraint_stakeholder(spatial_mismatch_mountain, municipal_legislator, agenda_setter,
    institutional, immediate, constrained, local).

% Observes the constraint from a civilizational/universal perspective; distinguishes the physical constraint (fixed curb length, growing vehicle fleet) from the policy constraint (allocation rule that determines who bears the scarcity cost). Sees the mountain classification as potentially valid (the physical constraint is genuine) but flags the beneficiary structure (early adopters, high-ownership households) as evidence of a false summit (the 'natural' scarcity is being used to justify an allocation rule that extracts from non-car-owners).
narrative_ontology:constraint_stakeholder(spatial_mismatch_mountain, analytical_observer, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(spatial_mismatch_mountain, early_vehicle_adopter).
narrative_ontology:fixing_cost_class(spatial_mismatch_mountain, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: The curb allocation system rations a fixed scarce resource (curb length) among competing users (vehicle owners). The coordination problem is genuine: without an allocation rule, curb space would be contested via first-come-first-served daily competition, creating search costs and conflict. The permit system solves this by assigning stable access rights.
% TRANSFER_FUNCTION: The arrangement transfers the opportunity cost of curb space from vehicle owners to non-vehicle-owners. Vehicle owners consume curb space at below-market rates (free or nominal permit fees); non-vehicle-owners subsidize this consumption via general taxes that fund street maintenance. The subsidy value scales with vehicle ownership: high-car-ownership households capture more curb space and more subsidy.
% ABSENT_VOICES: Non-car-owners and late-arriving residents (renters, recent immigrants, young adults) would object if present. They are excluded from the allocation conversation because they do not own vehicles (and thus have no standing in parking policy debates) or because they lack political power (renters do not vote in local elections at the same rates as homeowners). The homeowner constituency dominates municipal parking policy, and homeowners disproportionately own vehicles and benefit from free curb allocation.
% DISAPPEARANCE_RATIONALE: If the spatial mismatch constraint disappeared overnight (curb length magically expanded to match vehicle fleet), the world would rearrange: circling time would drop to zero, parking search costs would vanish, and the political conflict over curb allocation would dissolve. However, the constraint's disappearance is physically impossible (curb length is fixed by street geometry), so the verdict is contested: some observers (early adopters, municipal legislators) would say the world is unchanged because the scarcity is a natural law; other observers (transportation planners, non-car-owners) would say the world rearranges because the scarcity is partly policy-induced (free allocation creates excess demand that pricing would eliminate).
% FOUNDING_PROBLEM: The curb allocation system was built to solve the coordination problem of rationing scarce curb space among competing vehicle owners in dense urban areas. In the mid-20th century, as vehicle ownership grew and urban curb space became contested, cities implemented residential permit systems to assign stable access rights and prevent daily first-come-first-served competition.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem (rationing scarce curb space) is corroborated by transportation planners, urban economists, and municipal traffic engineers — all of whom observe that curb space is a fixed scarce resource requiring allocation rules. However, the STATUS of the problem (whether the scarcity is primarily physical or policy-induced) is contested: transportation planners argue that free allocation creates excess demand (the problem is partly policy-induced), while municipal legislators and homeowner advocates argue that the scarcity is primarily physical (curb length cannot grow, vehicle fleet will grow). The corroboration is thus partial: the founding problem is real, but its nature (physical versus policy) is disputed.
narrative_ontology:disappearance_verdict(spatial_mismatch_mountain, contested).
narrative_ontology:founding_problem_status(spatial_mismatch_mountain, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DAILY CIRCLER (MOUNTAIN) — Trapped in immediate search for parking; experiences the physical constraint as unchangeable natural law. Curb length is fixed, cars multiply, circling time grows. No exit from the geometry.
constraint_indexing:constraint_classification(spatial_mismatch_mountain, mountain,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SUBURBAN COMMUTER (MOUNTAIN) — Constrained by job location and housing costs; sees the parking scarcity as a fixed feature of urban density. Could relocate or change jobs at high cost, but the underlying geometry (fixed curb, growing fleet) appears immutable.
constraint_indexing:constraint_classification(spatial_mismatch_mountain, mountain,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: EARLY VEHICLE ADOPTER (ROPE) — Institutional actors (households that acquired vehicles when curb space was abundant) experience the constraint as coordination: the curb allocation system rationed a scarce resource on a first-come basis. They arrived early, claimed space, and now benefit from the lock-in. Low extraction because they are net beneficiaries of the allocation rule.
constraint_indexing:constraint_classification(spatial_mismatch_mountain, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: TRANSPORTATION PLANNER COALITION (TANGLED ROPE) — Organized agents (urban planners, transit advocates, congestion pricing proponents) see both the physical constraint (fixed curb) and the policy constraint (free allocation creates excess demand). They experience coordination (managing a commons) and extraction (political resistance to pricing reforms protects incumbent parkers at the expense of system efficiency). Mobile because they can advocate for policy change, but constrained by political economy.
constraint_indexing:constraint_classification(spatial_mismatch_mountain, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 5: ANALYTICAL OBSERVER (MOUNTAIN) — From a civilizational/universal perspective, the physical constraint is genuine: urban curb length is fixed by street geometry, and vehicle fleet growth is a demographic fact. The mismatch is a mountain — a structural feature of urbanization that no policy can eliminate, only manage. However, the beneficiary structure (early adopters, high-ownership households) reveals that the 'natural' scarcity is amplified by allocation rules. The analytical observer must distinguish the physical constraint (mountain) from the policy constraint (the allocation rule that determines who bears the scarcity cost).
constraint_indexing:constraint_classification(spatial_mismatch_mountain, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(spatial_mismatch_mountain_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(spatial_mismatch_mountain, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(spatial_mismatch_mountain, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(invariance_check) :-
    % Verify that as a Mountain, the classification is uniform across perspectives.
    constraint_indexing:constraint_classification(spatial_mismatch_mountain, TypeTarget, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(spatial_mismatch_mountain, TypeBeneficiary, context(agent_power(institutional), _, _, _)),
    TypeTarget == TypeBeneficiary,
    TypeTarget == mountain.

test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(spatial_mismatch_mountain, ExtMetricName, E),
    domain_priors:suppression_score(spatial_mismatch_mountain, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(spatial_mismatch_mountain),
    narrative_ontology:constraint_metric(spatial_mismatch_mountain, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(spatial_mismatch_mountain, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(spatial_mismatch_mountain_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.15): Low but non-zero. The physical constraint (fixed curb length) is genuine and unavoidable, but the allocation rule (free or underpriced permits) creates a subsidy that flows to early adopters and high-ownership households. The extraction is the opportunity cost of curb space allocated below market rate — a transfer from non-car-owners (who subsidize the curb via general taxes) to car owners (who consume the curb without paying marginal cost). The value is low because the physical constraint dominates: even with optimal pricing, some scarcity would remain. Suppression (0.20): Low. Alternatives to private vehicle storage exist (transit, car-sharing, micromobility, relocation) but are costly or inconvenient in low-density contexts. The suppression reflects the structural barriers to exit (job location, housing costs, transit availability) rather than active enforcement. Theater ratio (0.10): Very low. Parking enforcement and permit systems are functional, not performative — they allocate a real scarce resource. The theater component reflects the political rhetoric that frames the scarcity as purely natural (geometry) rather than partly policy-induced (allocation rule). Accessibility collapse (0.88): High. Once the physical constraint is understood (curb length is fixed, vehicles multiply), alternatives collapse nearly completely for most urban residents. Relocation, job change, and car-free living are possible but costly. Resistance (0.05): Very low. The physical constraint (fixed curb length) meets almost no resistance — it is a fact of geometry. The policy constraint (allocation rule) meets political resistance, but that resistance is to changing the rule, not to the rule's existence.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap is diagnostic. The daily circler sees a mountain (unchangeable geometry). The early adopter sees a rope (coordination that allocated a scarce resource fairly on a first-come basis). The transportation planner sees a tangled rope (genuine physical constraint plus policy-induced excess demand). The analytical observer sees a mountain but must distinguish the physical constraint (genuine) from the policy constraint (contingent). The gap reveals that the 'natural law' framing (fixed curb length) is being used to naturalize a policy choice (free allocation) that benefits incumbents. The false summit detector will flag this: a mountain with declared beneficiaries, rising extractiveness over time (as the fleet grows and scarcity intensifies), and a policy alternative (pricing) that would redistribute the scarcity cost.
 *
 * DIRECTIONALITY LOGIC:
 *   The directionality structure reveals the false summit signature. Early vehicle adopters and high-car-ownership households are declared beneficiaries — they captured curb space when it was abundant and now benefit from the allocation lock-in. The engine will derive low d values for these agents (beneficiary status + arbitrage exit options), producing low or negative effective extraction. Non-car-owners and late adopters are implicit victims — they bear the opportunity cost of underpriced curb allocation via general tax subsidy and reduced access to alternatives (transit, bike lanes) that could be funded by curb pricing revenue. The daily circler (powerless/trapped) experiences maximum extraction — no exit, no benefit, pure cost. The analytical observer sees a mountain (physical constraint is genuine) but must account for the beneficiary structure: if the 'natural' scarcity is being used to justify an allocation rule that extracts from non-car-owners, the mountain is a false summit.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandatrophy resolution hinges on the physical-versus-policy decomposition. If the scarcity is purely physical (curb length cannot grow, vehicle fleet will grow), the mountain holds — the constraint is a genuine natural law of urbanization. If the scarcity is partly policy-induced (free allocation creates excess demand that pricing would eliminate), the mountain is a false summit — the 'natural' constraint is being used to justify an allocation rule that extracts from non-car-owners. The omega variable 'physical_vs_policy_scarcity' is the resolution mechanism: natural experiments in cities with market-rate curb pricing versus free allocation will reveal how much of the scarcity is geometry versus policy. The beneficiary structure (early adopters, high-ownership households) is the signal: if identifiable agents benefit from the 'natural' scarcity, the mountain classification is suspect.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    physical_vs_policy_scarcity,
    'How much of the experienced parking scarcity is due to the physical constraint (fixed curb length) versus the policy constraint (free allocation creating excess demand)?',
    'Natural experiment: measure parking availability and circling time in cities with market-rate curb pricing versus free allocation, controlling for density and vehicle ownership rates. If scarcity persists under pricing, the constraint is primarily physical. If scarcity resolves, the constraint is primarily policy-induced.',
    'If primarily physical: mountain classification holds — the constraint is a genuine natural law of urban geometry. If primarily policy-induced: the mountain is a false summit — the ''natural'' scarcity naturalizes a policy choice (free allocation) that benefits early adopters.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(physical_vs_policy_scarcity, empirical, 'Decomposition of physical versus policy-induced parking scarcity').

omega_variable(
    beneficiary_naturalization,
    'Do early vehicle adopters and high-car-ownership households benefit from the spatial mismatch, or do they merely experience it less severely?',
    'Distributional analysis: compare the subsidy value (market rate of curb space minus actual cost paid) across income quintiles and vehicle ownership levels. If subsidy skews toward high-ownership households, they are beneficiaries. If subsidy is uniform or progressive, they are not.',
    'If beneficiaries exist: the mountain classification is a false summit — the ''natural'' constraint is being used to justify an allocation rule that extracts from non-car-owners and late adopters. If no beneficiaries: the mountain holds — everyone bears the scarcity cost equally.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(beneficiary_naturalization, empirical, 'Whether early adopters are beneficiaries or merely less-impacted victims').

omega_variable(
    alternative_collapse_threshold,
    'At what density threshold do alternatives to private vehicle storage (car-sharing, transit, micromobility) become structurally equivalent to private parking, collapsing the accessibility gap?',
    'Cross-city comparison: measure modal shift rates and accessibility scores in cities above and below various density thresholds. Identify the density at which non-car modes provide equivalent access to jobs, services, and social networks.',
    'If threshold is low (e.g., 10,000 ppl/sq mi): the mountain is surmountable at moderate density — alternatives exist and the constraint is a policy choice. If threshold is high (e.g., 30,000+ ppl/sq mi): the mountain is genuine for most cities — private vehicle storage remains structurally necessary below the threshold.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_collapse_threshold, empirical, 'Density threshold at which alternatives collapse the accessibility gap').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(spatial_mismatch_mountain, 0, 40).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(curb_spatial_tr_t0, spatial_mismatch_mountain, theater_ratio, 0, 0.05).
narrative_ontology:measurement(curb_spatial_tr_t10, spatial_mismatch_mountain, theater_ratio, 10, 0.06).
narrative_ontology:measurement(curb_spatial_tr_t20, spatial_mismatch_mountain, theater_ratio, 20, 0.07).
narrative_ontology:measurement(curb_spatial_tr_t30, spatial_mismatch_mountain, theater_ratio, 30, 0.09).
narrative_ontology:measurement(curb_spatial_tr_t40, spatial_mismatch_mountain, theater_ratio, 40, 0.1).

% Extraction over time
narrative_ontology:measurement(curb_spatial_be_t0, spatial_mismatch_mountain, base_extractiveness, 0, 0.05).
narrative_ontology:measurement(curb_spatial_be_t10, spatial_mismatch_mountain, base_extractiveness, 10, 0.08).
narrative_ontology:measurement(curb_spatial_be_t20, spatial_mismatch_mountain, base_extractiveness, 20, 0.1).
narrative_ontology:measurement(curb_spatial_be_t30, spatial_mismatch_mountain, base_extractiveness, 30, 0.12).
narrative_ontology:measurement(curb_spatial_be_t40, spatial_mismatch_mountain, base_extractiveness, 40, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(curb_spatial_su_t0, spatial_mismatch_mountain, suppression_requirement, 0, 0.1).
narrative_ontology:measurement(curb_spatial_su_t10, spatial_mismatch_mountain, suppression_requirement, 10, 0.12).
narrative_ontology:measurement(curb_spatial_su_t20, spatial_mismatch_mountain, suppression_requirement, 20, 0.15).
narrative_ontology:measurement(curb_spatial_su_t30, spatial_mismatch_mountain, suppression_requirement, 30, 0.18).
narrative_ontology:measurement(curb_spatial_su_t40, spatial_mismatch_mountain, suppression_requirement, 40, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(spatial_mismatch_mountain, resource_allocation).

% DUAL FORMULATION NOTE:
% The spatial mismatch constraint is a candidate for decomposition into two stories: (1) the physical constraint (fixed curb length vs. growing vehicle fleet), which is a genuine mountain, and (2) the allocation constraint (free or underpriced permits creating excess demand), which is a policy choice (tangled rope or snare depending on perspective). The current story conflates these, which is appropriate for the mountain hypothesis but may obscure the policy-induced component. If the false summit detector flags this constraint, decomposition is warranted.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
