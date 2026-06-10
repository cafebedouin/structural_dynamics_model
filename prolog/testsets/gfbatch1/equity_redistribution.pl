% ============================================================================
% CONSTRAINT STORY: equity_redistribution
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_equity_redistribution, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: equity_redistribution
 *   human_readable: Equity Redistribution Reading of Curb Space Allocation
 *   domain: urban_planning/public_resource_allocation/transportation_policy
 *
 * SUMMARY:
 *   The equity redistribution reading frames curb space allocation as a
 *   public resource distribution problem requiring compensatory transfers to
 *   non-beneficiaries. Under this reading, free or underpriced curb parking
 *   constitutes a regressive subsidy: car owners (who skew higher-income)
 *   capture valuable public space while non-car-owners (who skew
 *   lower-income) subsidize the infrastructure through general taxes but
 *   receive no benefit. The constraint's core claim is that legitimacy
 *   requires redistribution: either direct compensation (rebates, transit
 *   passes) or indirect compensation (revenue-funded transit/bike
 *   infrastructure improvements). This reading is one of three live framings
 *   of the same curb space allocation kernel. The property_tax_entitlement
 *   reading holds that car owners already paid for curb infrastructure
 *   through property taxes and are claiming a return on prior contribution.
 *   The public_resource_pricing reading holds that efficiency pricing
 *   (congestion-based, demand-responsive) is sufficient without
 *   redistribution—let the market clear and revenue is incidental. The
 *   equity_redistribution reading holds that pricing without redistribution
 *   perpetuates extraction because the subsidy skew is the injustice, not
 *   merely the mispricing.
 *
 * KEY AGENTS:
 *   - Car Owners: Primary beneficiaries (institutional/arbitrage) — capture $2000-5000/year subsidy in high-demand areas through free or underpriced curb access
 *   - Non-Car-Owners (Modal Choice): Secondary victims (moderate/constrained) — subsidize car infrastructure through property taxes; could exit by buying car but would lose environmental/health benefits
 *   - Transit-Dependent Low-Income Residents: Primary victims (powerless/trapped) — cannot afford car ownership; trapped in underserved areas; pay full extraction through reduced mobility and opportunity costs
 *   - Municipal Transportation Department: Institutional actor (institutional/constrained) — manages allocation but politically captured by car-owner voting bloc; constrained from implementing pricing reforms
 *   - Transit Advocacy Coalition: Organized reformers (organized/mobile) — building political coalition for pricing and redistribution; see sunset path through climate policy and density increases
 *   - Commercial Delivery Operators: Secondary beneficiaries (institutional/arbitrage) — capture curb access for loading zones; benefit from underpriced allocation
 *   - Ride-Hail Platforms: Secondary beneficiaries (institutional/arbitrage) — capture curb access for passenger pickup/dropoff; externalize congestion costs
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(equity_redistribution, 0.68).
domain_priors:suppression_score(equity_redistribution, 0.72).
domain_priors:theater_ratio(equity_redistribution, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(equity_redistribution, extractiveness, 0.68).
narrative_ontology:constraint_metric(equity_redistribution, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(equity_redistribution, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(equity_redistribution, accessibility_collapse, 0.35).
narrative_ontology:constraint_metric(equity_redistribution, resistance, 0.78).

% --- Constraint claim ---
narrative_ontology:constraint_claim(equity_redistribution, tangled_rope).
narrative_ontology:human_readable(equity_redistribution, "Equity Redistribution Reading of Curb Space Allocation").
narrative_ontology:topic_domain(equity_redistribution, "urban_planning/public_resource_allocation/transportation_policy").

domain_priors:requires_active_enforcement(equity_redistribution).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(equity_redistribution, '325e77fe-d2d0-47c1-9591-3fff02f09e21').
narrative_ontology:cs_kernel_codification('325e77fe-d2d0-47c1-9591-3fff02f09e21', distributed).
narrative_ontology:cs_authority_grounding('325e77fe-d2d0-47c1-9591-3fff02f09e21', distributed).
narrative_ontology:cs_reading_relation('325e77fe-d2d0-47c1-9591-3fff02f09e21', equity_redistribution__property_tax_entitlement, coexists_with).
narrative_ontology:cs_reading_relation('325e77fe-d2d0-47c1-9591-3fff02f09e21', equity_redistribution__public_resource_pricing, influences).
narrative_ontology:cs_axiom('325e77fe-d2d0-47c1-9591-3fff02f09e21', foundational, subsidy_skew_requires_compensation).
narrative_ontology:cs_axiom_status(subsidy_skew_requires_compensation, holdable).
narrative_ontology:cs_axiom_grounding('325e77fe-d2d0-47c1-9591-3fff02f09e21', subsidy_skew_requires_compensation, deontological).
narrative_ontology:cs_axiom('325e77fe-d2d0-47c1-9591-3fff02f09e21', secondary, public_resource_access_equity).
narrative_ontology:cs_axiom_status(public_resource_access_equity, holdable).
narrative_ontology:cs_axiom_grounding('325e77fe-d2d0-47c1-9591-3fff02f09e21', public_resource_access_equity, conventional).
narrative_ontology:cs_reference_frame('325e77fe-d2d0-47c1-9591-3fff02f09e21', progressive_resource_allocation_norm).
narrative_ontology:cs_drift_state('325e77fe-d2d0-47c1-9591-3fff02f09e21', contemporary_climate_policy_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('325e77fe-d2d0-47c1-9591-3fff02f09e21', '2026-02-26T14:32:00Z').
narrative_ontology:cs_kernel_id(equity_redistribution, curb_space_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(equity_redistribution, car_owners).
narrative_ontology:constraint_beneficiary(equity_redistribution, commercial_delivery_operators).
narrative_ontology:constraint_beneficiary(equity_redistribution, ride_hail_platforms).
narrative_ontology:constraint_victim(equity_redistribution, non_car_owners).
narrative_ontology:constraint_victim(equity_redistribution, low_income_residents).
narrative_ontology:constraint_victim(equity_redistribution, transit_dependent_households).
narrative_ontology:constraint_victim(equity_redistribution, pedestrian_realm).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(equity_redistribution, car_owners_high_demand_districts).
narrative_ontology:constraint_victim(equity_redistribution, non_car_owners_modal_choice).
narrative_ontology:constraint_victim(equity_redistribution, transit_dependent_low_income).
narrative_ontology:constraint_vindicates(equity_redistribution, progressive_taxation_principle).
narrative_ontology:constraint_vindicates(equity_redistribution, public_resource_equity_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Capture free or underpriced curb parking worth $2000-5000/year in market value. Can choose paid private parking if curb access degrades, or relocate to lower-demand area. Organized voting bloc with political influence over municipal parking policy.
narrative_ontology:constraint_stakeholder(equity_redistribution, car_owners_high_demand_districts, beneficiary,
    institutional, immediate, arbitrage, local).

% Subsidize curb infrastructure through property taxes but receive no direct benefit. Could buy car to access subsidy but would lose environmental and health benefits of car-free lifestyle. Benefit from reduced congestion when others use transit, but this is diffuse and uncompensated.
narrative_ontology:constraint_stakeholder(equity_redistribution, non_car_owners_modal_choice, payer,
    moderate, biographical, constrained, regional).

% Cannot afford car ownership. Trapped in neighborhoods where curb space subsidizes car storage while transit service degrades from underinvestment. Pay full extraction through reduced mobility, longer commutes, and opportunity costs. Cannot relocate to better-served areas (too expensive) or afford car ownership (income-constrained).
narrative_ontology:constraint_stakeholder(equity_redistribution, transit_dependent_low_income, payer,
    powerless, biographical, trapped, local).

% Manages curb allocation but faces political pressure to maintain free parking. Cannot unilaterally implement pricing without council approval. Caught between coordination function (managing finite resource) and extraction mechanism (politically captured allocation). Produces equity studies but lacks authority to implement redistribution.
narrative_ontology:constraint_stakeholder(equity_redistribution, municipal_transportation_department, agenda_setter,
    institutional, biographical, constrained, regional).

% Organized groups (transit riders unions, bike coalitions, pedestrian advocates) building political coalition for pricing and redistribution. See current allocation as temporary injustice with sunset path through climate policy and density increases. Excluded from initial allocation decisions but gaining influence through ballot measures and council advocacy.
narrative_ontology:constraint_stakeholder(equity_redistribution, transit_advocacy_coalition, excluded,
    organized, generational, mobile, regional).

% Capture curb access for loading zones at below-market rates. Benefit from underpriced allocation that externalizes congestion costs. Can negotiate with municipalities for dedicated loading zones or use private loading facilities if curb access degrades.
narrative_ontology:constraint_stakeholder(equity_redistribution, commercial_delivery_operators, beneficiary,
    institutional, immediate, arbitrage, regional).

% Analytical observer measuring subsidy flows and allocation efficiency. Documents that curb space allocation has genuine coordination function but current mechanism embeds substantial extraction. Tracks implementation of pricing and redistribution reforms across cities.
narrative_ontology:constraint_stakeholder(equity_redistribution, urban_planning_analyst, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(equity_redistribution, car_owners_high_demand_districts).
narrative_ontology:fixing_cost_class(equity_redistribution, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Curb space allocation solves the real problem of managing finite linear footage among competing uses: vehicle storage, loading zones, transit stops, bike lanes, pedestrian space, street furniture. Without some allocation mechanism, curb access would be chaotic and contested.
% TRANSFER_FUNCTION: The arrangement transfers wealth from general tax base (including non-car-owners) to car owners through underpriced or free curb parking. Annual subsidy value ranges $2000-5000 per vehicle in high-demand areas. Also transfers public space (opportunity cost of land) from potential alternative uses (wider sidewalks, bike lanes, parklets, transit lanes) to private vehicle storage.
% ABSENT_VOICES: Transit-dependent low-income residents are systematically underrepresented in curb allocation decisions. They lack organized advocacy groups with municipal access, face language and time barriers to public comment processes, and are concentrated in neighborhoods with less political influence. Also absent: future residents of denser, less car-dependent neighborhoods that current allocation patterns prevent from existing.
% DISAPPEARANCE_RATIONALE: If curb space allocation disappeared overnight (no rules governing use), immediate chaos would force rapid rearrangement: private property owners would claim adjacent curb space, commercial operators would monopolize loading zones, conflicts would escalate until new allocation mechanism emerged (likely more extractive and less coordinated than current system). The constraint's disappearance would not leave the world unchanged—it would trigger reallocation through other mechanisms.
% FOUNDING_PROBLEM: Curb space allocation was built to solve the problem of managing vehicle access in dense urban areas as car ownership expanded in the mid-20th century. The founding problem was genuine coordination: without parking rules, vehicles would block traffic, emergency access, and commercial loading. Early parking regulations (time limits, permit systems) were coordination mechanisms to enable vehicle access while preventing monopolization.
% FOUNDING_PROBLEM_CORROBORATION: The founding problem's status is contested. Transportation engineers and urban planners attest that coordination function remains live: curb space must be allocated among competing uses. But the SCALE of the problem has changed: car ownership rates have stabilized or declined in many cities, while demand for alternative uses (bike lanes, transit lanes, pedestrian space) has increased. Transit advocates and environmental groups argue the founding problem is dead: the constraint now solves a problem (vehicle storage) that is less urgent than the problems it creates (congestion, pollution, inequitable subsidy). Property rights advocates argue the founding problem is live and that car owners have legitimate claim to curb access based on prior infrastructure investment.
narrative_ontology:disappearance_verdict(equity_redistribution, world_rearranges).
narrative_ontology:founding_problem_status(equity_redistribution, contested).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: TRANSIT-DEPENDENT LOW-INCOME RESIDENT (SNARE) — Cannot afford car ownership; trapped in neighborhoods where curb space subsidizes car storage while transit service degrades from underinvestment. Pays full extraction through reduced mobility, longer commutes, and opportunity costs. No exit: cannot move to car-lite neighborhoods (too expensive) or afford car ownership (income-constrained). Maximum experienced extraction.
constraint_indexing:constraint_classification(equity_redistribution, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: NON-CAR-OWNER WITH MODAL CHOICE (TANGLED ROPE) — Has income to own car but chooses not to; benefits from reduced congestion and pollution when others use transit, but subsidizes car owners through property taxes that fund curb infrastructure. Constrained exit: could buy car to access subsidy but would lose environmental/health benefits of car-free lifestyle. Mixed experience: coordination function exists (curb space does enable goods delivery and emergency access) but asymmetric extraction is substantial.
constraint_indexing:constraint_classification(equity_redistribution, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: CAR OWNER IN HIGH-DEMAND DISTRICT (ROPE) — Captures substantial subsidy: free or underpriced curb parking worth $2000-5000/year in market value. Experiences constraint as pure coordination: curb space enables vehicle storage near destinations. Arbitrage exit: can choose paid private parking if curb access degrades, or relocate to lower-demand area. Net beneficiary with high mobility.
constraint_indexing:constraint_classification(equity_redistribution, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(local))).

% PERSPECTIVE 4: TRANSIT ADVOCACY COALITION (SCAFFOLD) — Organized groups (transit riders unions, bike coalitions, pedestrian advocates) see current allocation as temporary injustice with clear sunset path: congestion pricing, parking benefit districts, and curb management reforms are spreading across cities. Sunset mechanism: as climate policy and housing density increase, political coalitions shift toward pricing car access and redistributing revenue. Estimated timeline: 15-25 years for norm shift in major metros.
constraint_indexing:constraint_classification(equity_redistribution, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 5: MUNICIPAL TRANSPORTATION DEPARTMENT (TANGLED ROPE) — Institutional actor caught between coordination function (managing curb access for multiple uses) and extraction mechanism (political pressure to maintain free parking subsidizes car owners at expense of general fund). Constrained exit: cannot unilaterally implement pricing without council approval; faces organized opposition from car-owner voting bloc. Experiences both coordination need (curb space must be allocated somehow) and extraction (current allocation is politically captured, not efficiency-optimized).
constraint_indexing:constraint_classification(equity_redistribution, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — From analytical perspective, curb space allocation has genuine coordination function (finite resource must be allocated among competing uses) but current mechanism embeds substantial extraction (subsidy flows from general tax base to car owners; non-car-owners pay but don't benefit). Not a mountain: allocation rules are policy choices, not natural laws. Not pure snare: coordination problem is real. Tangled rope: both functions coexist and are structurally inseparable under current institutional arrangements.
constraint_indexing:constraint_classification(equity_redistribution, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(equity_redistribution_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(equity_redistribution, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(equity_redistribution, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(equity_redistribution, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(equity_redistribution_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.68): High. The subsidy skew is substantial and regressive. Market-rate parking in dense urban areas ranges $15-30/day; annual value of free curb space is $2000-5000 per vehicle. Non-car-owners subsidize this through property taxes (which fund street maintenance including curb infrastructure) but receive no benefit. The extraction has increased over the interval as urban density and parking demand have grown, making the opportunity cost of curb space higher. Suppression (0.72): High. Non-car-owners face significant barriers to contesting the allocation: car owners are organized voting bloc; municipal revenue depends on car-related fees and fines; alternative transportation infrastructure is underfunded; zoning laws mandate parking minimums that entrench car dependency. Suppression has increased as car ownership has become more entrenched in land use patterns. Theater ratio (0.45): Moderate. Some performative elements exist (equity studies commissioned but not acted upon; pilot programs that don't scale; community engagement processes that don't change outcomes) but the constraint is not primarily theatrical—the extraction is real and the enforcement is functional. Theater has increased as cities have adopted equity rhetoric without implementing redistribution. Accessibility collapse (0.35): Low-moderate. Alternatives to car-centric curb allocation exist and are visible: congestion pricing (London, Singapore, Stockholm), parking benefit districts (San Francisco, Seattle), complete streets redesigns, transit-oriented development. The constraint does not collapse alternatives—it suppresses them through political capture. Resistance (0.78): High. The constraint faces substantial organized resistance from transit advocates, environmental groups, housing advocates, and increasingly from climate policy coalitions. Resistance is active and growing, not latent.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates how the same curb space allocation produces radically different experiences based on structural position. Car owners see pure coordination—curb space enables vehicle storage and access. Transit-dependent residents see pure extraction—they subsidize infrastructure they cannot use while their mobility options degrade. Non-car-owners with choice see mixed coordination and extraction—the system does solve real allocation problems but embeds regressive subsidy. The municipal department sees institutional capture—coordination function exists but is subordinated to extraction mechanism through political pressure. The transit advocacy coalition sees a temporary injustice with sunset path—current extraction is being corrected through pricing and redistribution reforms. The analytical observer sees tangled rope—coordination and extraction are structurally inseparable under current institutional arrangements, and neither can be removed without transforming the constraint entirely.
 *
 * DIRECTIONALITY LOGIC:
 *   Car owners are primary beneficiaries with arbitrage exit options—they experience the constraint as pure coordination (rope classification) because extraction flows toward them. Their directionality is near 0.0 (full beneficiary), producing negative effective extraction (they are subsidized). Non-car-owners with modal choice are secondary victims with constrained exit—they could buy a car to access the subsidy but would lose other benefits. Their directionality is moderate (~0.6), producing moderate effective extraction. Transit-dependent low-income residents are primary victims with trapped exit—they cannot afford car ownership and cannot relocate to better-served areas. Their directionality is near 1.0 (full target), producing maximum effective extraction (snare classification). The municipal transportation department is an institutional actor with constrained exit—it experiences both coordination function and extraction mechanism, producing tangled rope classification with moderate directionality (~0.5). The analytical observer sees the structural inseparability of coordination and extraction, also producing tangled rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   The equity redistribution reading resolves mandatrophy by asserting that the constraint's legitimacy depends on compensation to victims. Under this reading, curb space allocation without redistribution is extractive (tangled rope or snare depending on perspective), but allocation WITH redistribution can be legitimate coordination (rope or scaffold). The mandate is not 'allocate curb space' (which is unavoidable) but 'allocate curb space equitably' (which requires active redistribution). The reading distinguishes itself from the property_tax_entitlement reading (which holds that no redistribution is needed because car owners already paid) and the public_resource_pricing reading (which holds that efficiency pricing is sufficient without redistribution). The mandatrophy is resolved by making redistribution the legitimacy condition: if compensation mechanisms are effective, the constraint transitions toward scaffold; if they are ineffective or absent, it remains tangled rope or snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    subsidy_quantification_method,
    'What methodology accurately measures the subsidy value: opportunity cost of land, market rate for private parking, or public infrastructure maintenance cost?',
    'Comparative analysis across cities with different pricing regimes; hedonic pricing studies of curb access value; infrastructure cost accounting that separates curb maintenance from general street maintenance',
    'If opportunity cost method: subsidy estimates are highest ($3000-5000/year per space in dense areas), strengthening extraction claim. If maintenance cost method: subsidy estimates are lower ($500-1000/year), weakening extraction claim but still substantial.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subsidy_quantification_method, empirical, 'Methodology for quantifying curb parking subsidy value').

omega_variable(
    redistribution_mechanism_effectiveness,
    'Do revenue-neutral redistribution mechanisms (transit passes, bike infrastructure, pedestrian improvements funded by parking revenue) actually compensate non-car-owners, or do benefits accrue to different populations?',
    'Spatial analysis of who pays (parking fees by neighborhood income) vs who benefits (transit service improvements by neighborhood); longitudinal tracking of modal shift and accessibility changes after parking benefit district implementation',
    'If compensation is effective and reaches victims: constraint shifts toward scaffold (temporary extraction being corrected). If compensation is ineffective or misdirected: remains tangled rope or snare (extraction persists despite reform theater).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(redistribution_mechanism_effectiveness, empirical, 'Whether redistribution mechanisms effectively compensate victims').

omega_variable(
    kernel_reading_under_determination,
    'Is the equity_redistribution reading the structurally correct framing, or does the property_tax_entitlement reading (car owners paid for infrastructure through property taxes) or public_resource_pricing reading (efficiency pricing without redistribution) better capture the legitimacy structure?',
    'Historical analysis of how curb infrastructure was funded (general obligation bonds vs gas taxes vs property taxes); legal analysis of whether property ownership confers curb access rights; cross-city comparison of pricing regimes and their political sustainability',
    'If property_tax_entitlement reading is correct: current allocation is not extraction but return on prior payment, changing classification toward rope. If public_resource_pricing reading is correct: redistribution is unnecessary; efficiency pricing alone resolves the constraint. If equity_redistribution reading is correct: compensation to non-car-owners is structural requirement for legitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_under_determination, conceptual, 'Which kernel reading correctly frames curb space legitimacy').

omega_variable(
    political_coalition_stability,
    'Are the political coalitions supporting equity redistribution (transit advocates, environmental groups, housing advocates) stable enough to sustain reform through implementation, or will they fragment when redistribution costs become concrete?',
    'Analysis of coalition behavior in cities that have implemented parking pricing and revenue redistribution; identification of defection points and coalition maintenance mechanisms; comparison to other progressive taxation reforms',
    'If coalitions are stable: scaffold perspective is validated (sunset is real). If coalitions fragment: reform stalls and constraint remains tangled rope or reverts to snare as enforcement weakens.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(political_coalition_stability, empirical, 'Stability of political coalitions supporting redistribution').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(equity_redistribution, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(curb_equity_tr_t0, equity_redistribution, theater_ratio, 0, 0.3).
narrative_ontology:measurement(curb_equity_tr_t3, equity_redistribution, theater_ratio, 3, 0.35).
narrative_ontology:measurement(curb_equity_tr_t6, equity_redistribution, theater_ratio, 6, 0.42).
narrative_ontology:measurement(curb_equity_tr_t10, equity_redistribution, theater_ratio, 10, 0.45).

% Extraction over time
narrative_ontology:measurement(curb_equity_be_t0, equity_redistribution, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(curb_equity_be_t3, equity_redistribution, base_extractiveness, 3, 0.6).
narrative_ontology:measurement(curb_equity_be_t6, equity_redistribution, base_extractiveness, 6, 0.65).
narrative_ontology:measurement(curb_equity_be_t10, equity_redistribution, base_extractiveness, 10, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(curb_equity_su_t0, equity_redistribution, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(curb_equity_su_t3, equity_redistribution, suppression_requirement, 3, 0.65).
narrative_ontology:measurement(curb_equity_su_t6, equity_redistribution, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(curb_equity_su_t10, equity_redistribution, suppression_requirement, 10, 0.72).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(equity_redistribution, resource_allocation).
narrative_ontology:affects_constraint(equity_redistribution, parking_minimum_zoning).
narrative_ontology:affects_constraint(equity_redistribution, transit_funding_allocation).
narrative_ontology:affects_constraint(equity_redistribution, complete_streets_implementation).

% DUAL FORMULATION NOTE:
% The equity_redistribution reading is one of three framings of curb space legitimacy. The property_tax_entitlement and public_resource_pricing readings are sibling constraints (separate JSON files) linked via network.affects_constraints. Each reading has its own epsilon value reflecting different beneficiary/victim structures under different legitimacy claims.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
