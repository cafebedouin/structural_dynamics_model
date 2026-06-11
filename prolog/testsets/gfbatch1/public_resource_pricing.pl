% ============================================================================
% CONSTRAINT STORY: public_resource_pricing
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-01-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_public_resource_pricing, []).

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
 *   constraint_id: public_resource_pricing
 *   human_readable: Cost-Recovery Pricing for Curb Space Allocation
 *   domain: urban_planning/public_resource_allocation/transportation_policy
 *
 * SUMMARY:
 *   Cost-recovery pricing for curb space allocation represents a contested
 *   reading of public infrastructure governance. This constraint instantiates
 *   the 'public resource pricing' reading of the curb space legitimacy
 *   kernel, where scarce public infrastructure requires market-rate or
 *   cost-recovery fees to manage demand efficiently. Sibling readings include
 *   'property tax entitlement' (residents have already paid for curb access
 *   through property taxes) and 'equity redistribution' (curb space should be
 *   allocated to maximize mobility access for disadvantaged populations). The
 *   structural delta for this reading: permit fees at or near market-clearing
 *   rates, caps on permits per household, and revenue recycling into
 *   alternative mobility infrastructure. The constraint exhibits genuine
 *   coordination function (reduces cruising, funds transit and bike
 *   infrastructure, improves commercial loading access) alongside asymmetric
 *   extraction (low-income car-dependent households face new costs with no
 *   viable alternative, suburban commuters pay for previously free access).
 *   The extractiveness has increased over the 12-year interval as permit fees
 *   have risen faster than income growth and as enforcement has intensified.
 *   Theater ratio remains low (0.28) because the pricing mechanism is
 *   functionally effective at managing demand — it is not performative.
 *   Suppression has increased as enforcement capacity has grown and as
 *   alternative parking options have been eliminated.
 *
 * KEY AGENTS:
 *   - Low-Income Car-Dependent Households: Primary victim (powerless/trapped) — cannot afford permits, no transit alternative, residential location locked by housing costs
 *   - Suburban Commuters: Secondary victim (moderate/constrained) — can afford permits but experience them as extraction; benefit from reduced competition
 *   - Municipal Revenue Authority: Primary beneficiary (institutional/arbitrage) — captures permit revenue, funds budget priorities, experiences as pure coordination
 *   - Alternative Mobility Coalition: Secondary beneficiary (organized/mobile) — cyclists, transit users, micromobility operators benefit from curb reallocation and revenue-funded infrastructure
 *   - Commercial Delivery Sector: Mixed position (institutional/mobile) — benefits from predictable loading access, pays new permit costs
 *   - Equity-Focused Reform Coalition: Organized agents (organized/constrained) — see pricing as transitional mechanism with sunset once transit density reaches threshold
 *   - Analytical Observer: Civilizational view (analytical/analytical) — sees both genuine coordination and asymmetric extraction; not a false summit
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(public_resource_pricing, 0.35).
domain_priors:suppression_score(public_resource_pricing, 0.45).
domain_priors:theater_ratio(public_resource_pricing, 0.28).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(public_resource_pricing, extractiveness, 0.35).
narrative_ontology:constraint_metric(public_resource_pricing, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(public_resource_pricing, theater_ratio, 0.28).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(public_resource_pricing, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(public_resource_pricing, resistance, 0.65).

% --- Constraint claim ---
narrative_ontology:constraint_claim(public_resource_pricing, tangled_rope).
narrative_ontology:human_readable(public_resource_pricing, "Cost-Recovery Pricing for Curb Space Allocation").
narrative_ontology:topic_domain(public_resource_pricing, "urban_planning/public_resource_allocation/transportation_policy").

domain_priors:requires_active_enforcement(public_resource_pricing).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(public_resource_pricing, '3e222097-ee02-4a2f-b9c3-21498bc6244b').
narrative_ontology:cs_kernel_codification('3e222097-ee02-4a2f-b9c3-21498bc6244b', formalized).
narrative_ontology:cs_authority_grounding('3e222097-ee02-4a2f-b9c3-21498bc6244b', extraction).
narrative_ontology:cs_interpretation_layer_present('3e222097-ee02-4a2f-b9c3-21498bc6244b').
narrative_ontology:cs_reading_relation('3e222097-ee02-4a2f-b9c3-21498bc6244b', public_resource_pricing__property_tax_entitlement, coexists_with).
narrative_ontology:cs_reading_relation('3e222097-ee02-4a2f-b9c3-21498bc6244b', public_resource_pricing__equity_redistribution, influences).
narrative_ontology:cs_axiom('3e222097-ee02-4a2f-b9c3-21498bc6244b', foundational, scarcity_requires_price_rationing).
narrative_ontology:cs_axiom_status(scarcity_requires_price_rationing, holdable).
narrative_ontology:cs_axiom_grounding('3e222097-ee02-4a2f-b9c3-21498bc6244b', scarcity_requires_price_rationing, empirically_contingent).
narrative_ontology:cs_axiom('3e222097-ee02-4a2f-b9c3-21498bc6244b', foundational, user_pays_principle).
narrative_ontology:cs_axiom_status(user_pays_principle, holdable).
narrative_ontology:cs_axiom_grounding('3e222097-ee02-4a2f-b9c3-21498bc6244b', user_pays_principle, conventional).
narrative_ontology:cs_reference_frame('3e222097-ee02-4a2f-b9c3-21498bc6244b', market_efficiency_allocation).
narrative_ontology:cs_drift_state('3e222097-ee02-4a2f-b9c3-21498bc6244b', contemporary_equity_challenge, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('3e222097-ee02-4a2f-b9c3-21498bc6244b', '2026-01-15T14:32:00Z').
narrative_ontology:cs_kernel_id(public_resource_pricing, curb_space_legitimacy).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(public_resource_pricing, municipal_revenue_authority).
narrative_ontology:constraint_beneficiary(public_resource_pricing, alternative_mobility_users).
narrative_ontology:constraint_beneficiary(public_resource_pricing, commercial_delivery_operators).
narrative_ontology:constraint_victim(public_resource_pricing, low_income_car_dependent_households).
narrative_ontology:constraint_victim(public_resource_pricing, suburban_commuters).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(public_resource_pricing, commercial_delivery_operators).
narrative_ontology:constraint_vindicates(public_resource_pricing, market_efficiency_doctrine).
narrative_ontology:constraint_vindicates(public_resource_pricing, congestion_pricing_theory).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Households earning below 80% area median income, living in neighborhoods with sparse transit coverage (headways > 30 minutes), requiring car access for work commutes or caregiving responsibilities. Cannot afford annual permit fees of $500-$1500 (5-15% of discretionary income). No viable alternative: relocating closer to transit requires higher rent; transit commute times are 2-3x car commute times; cycling is unsafe on arterial routes. Experience permit pricing as exclusion from a resource their property taxes built.
narrative_ontology:constraint_stakeholder(public_resource_pricing, low_income_car_dependent_households, payer,
    powerless, immediate, trapped, local).

% Households living outside the urban core, commuting to jobs in permit-priced districts. Can afford permit fees but experience them as a new tax on previously free access. Benefit from reduced cruising time when permits limit competition (average search time drops from 8 minutes to 3 minutes). Could relocate closer to work or shift to transit, but at significant cost (housing price premium, longer total commute including walk time). Pay $800-$2000 annually in permit fees.
narrative_ontology:constraint_stakeholder(public_resource_pricing, suburban_commuters, payer,
    moderate, biographical, constrained, regional).

% City transportation department or parking authority that sets permit fees, enforces compliance, and allocates revenue. Captures $50-$200 million annually in permit revenue (varies by city size). Uses revenue to fund transit expansion, bike infrastructure, and general transportation budget. Experiences permit pricing as pure coordination: solves the allocation problem, reduces cruising-related congestion, and generates budget flexibility. Can adjust fee structure, enforcement intensity, and revenue allocation with minimal external constraint.
narrative_ontology:constraint_stakeholder(public_resource_pricing, municipal_revenue_authority, agenda_setter,
    institutional, generational, arbitrage, national).
narrative_ontology:stakeholder_secondary_role(public_resource_pricing, municipal_revenue_authority, beneficiary).

% Cyclists, transit users, micromobility operators (bike-share, scooter-share) who benefit from curb space reallocation and revenue-funded infrastructure. Permit pricing creates political and fiscal space for protected bike lanes, bus-only lanes, and micromobility parking corrals that were previously occupied by free car storage. Organized through advocacy coalitions (bike coalitions, transit rider unions). Can shift between modes or relocate with relatively low cost. Receive $20-$80 million annually in infrastructure investment funded by permit revenue.
narrative_ontology:constraint_stakeholder(public_resource_pricing, alternative_mobility_users, beneficiary,
    organized, biographical, mobile, local).

% Logistics companies, freight carriers, and last-mile delivery services that require curb access for loading. Pay commercial permit fees ($2000-$10000 annually per vehicle) that were previously externalized. Benefit from predictable loading zone availability and reduced double-parking enforcement (which previously cost $500-$2000 per vehicle annually in fines and delay). Can shift delivery times, consolidate routes, or pass costs to customers. Mixed experience: coordination function is real (loading access improves), but extraction is also real (new cost burden).
narrative_ontology:constraint_stakeholder(public_resource_pricing, commercial_delivery_operators, payer,
    institutional, biographical, mobile, local).
narrative_ontology:stakeholder_secondary_role(public_resource_pricing, commercial_delivery_operators, beneficiary).

% Advocacy organizations, policy researchers, and community groups focused on transportation equity. See cost-recovery pricing as a transitional mechanism with a sunset: once transit density reaches threshold levels (15-minute headways on 3+ routes within 400m of 80% of residents) and residential parking is decoupled from housing mandates, the pricing mechanism becomes unnecessary. Push for means-tested discounts, revenue recycling into transit, and sunset clauses. Constrained by political feasibility and budget realities. Estimated sunset: 15-25 years as transit-oriented development matures.
narrative_ontology:constraint_stakeholder(public_resource_pricing, equity_focused_reform_coalition, observer,
    organized, generational, constrained, regional).

% Researchers, policy analysts, and systems thinkers examining curb space governance across jurisdictions and time horizons. See both genuine coordination function (pricing does manage scarce resources, does reduce cruising, does fund alternatives) and asymmetric extraction (low-income households are excluded, burden is regressive). Not a false summit — the coordination is real, but so is the extraction. The constraint is genuinely Tangled Rope at the analytical level.
narrative_ontology:constraint_stakeholder(public_resource_pricing, analytical_observer, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(public_resource_pricing, municipal_revenue_authority).
narrative_ontology:fixing_cost_class(public_resource_pricing, cheap).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Manages demand for scarce curb space in dense urban areas where parking supply is structurally insufficient relative to car ownership. Reduces cruising time (vehicles searching for parking), which accounts for 30-40% of congestion in commercial districts. Enables curb reallocation to higher-value uses (loading zones, bus lanes, bike corrals).
% TRANSFER_FUNCTION: Transfers money from car owners (especially suburban commuters and low-income car-dependent households) to municipal revenue authority. Transfers curb space from private car storage to alternative mobility infrastructure (bike lanes, bus lanes, loading zones). Transfers time from cruising drivers (who benefit from reduced search time) to municipal enforcement capacity (which must monitor compliance).
% ABSENT_VOICES: Low-income car-dependent households in neighborhoods with sparse transit coverage are systematically underrepresented in permit pricing debates. They lack organized advocacy capacity, face language and information barriers, and are often excluded from public comment processes that occur during work hours. Suburban commuters are present but politically weaker than urban residents. Future generations who would benefit from transit investment are absent by definition.
% DISAPPEARANCE_RATIONALE: If permit pricing disappeared overnight, curb space allocation would revert to time-based competition (cruising for parking), cruising-related congestion would increase by 30-40% in commercial districts, municipal revenue would drop by $50-$200 million annually (forcing cuts to transit or other services), and curb space currently allocated to bike lanes and loading zones would face political pressure to revert to free car storage. The world rearranges because multiple stakeholders' arrangements depend on the pricing mechanism.
% FOUNDING_PROBLEM: Curb space scarcity in dense urban areas where parking demand exceeds supply, leading to cruising congestion, unpredictable access for commercial loading, and political conflict over curb allocation. The founding problem emerged in the 1970s-1990s as car ownership grew faster than urban land supply.
% FOUNDING_PROBLEM_CORROBORATION: Curb space scarcity is corroborated by traffic engineers (who measure cruising time and parking occupancy rates), commercial delivery operators (who document loading access delays), and urban planners (who calculate parking supply/demand ratios). The problem is not contested — even critics of permit pricing acknowledge that curb space is scarce. The dispute is over the legitimacy of pricing as the allocation mechanism, not over whether scarcity exists.
narrative_ontology:disappearance_verdict(public_resource_pricing, world_rearranges).
narrative_ontology:founding_problem_status(public_resource_pricing, live).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: CAR-DEPENDENT LOW-INCOME HOUSEHOLD (SNARE) — Trapped by residential location and job accessibility; cannot afford permit fees that middle-class households absorb; no viable transit alternative exists in their neighborhood. Experiences maximum extraction with no coordination benefit — the pricing mechanism excludes them from a resource their tax dollars built.
constraint_indexing:constraint_classification(public_resource_pricing, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: SUBURBAN COMMUTER (TANGLED ROPE) — Constrained by residential choice and workplace location; can afford permits but experiences them as a new tax on previously free access. Benefits from reduced cruising time when permits limit competition, but pays substantially more than under the prior regime. Mixed coordination and extraction.
constraint_indexing:constraint_classification(public_resource_pricing, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: MUNICIPAL REVENUE AUTHORITY (ROPE) — Primary beneficiary capturing permit revenue and congestion-reduction benefits. Experiences the constraint as pure coordination: pricing solves the allocation problem, funds alternative mobility, and generates budget flexibility. Net beneficiary with arbitrage-level exit options.
constraint_indexing:constraint_classification(public_resource_pricing, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: ALTERNATIVE MOBILITY COALITION (ROPE) — Cyclists, transit users, micromobility operators who benefit from curb space reallocation and revenue-funded infrastructure. Organized agents with mobile exit options see genuine coordination: pricing creates space for bike lanes, bus lanes, and loading zones that were previously occupied by free car storage.
constraint_indexing:constraint_classification(public_resource_pricing, rope,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 5: EQUITY-FOCUSED REFORM COALITION (SCAFFOLD) — Sees cost-recovery pricing as a transitional mechanism with a sunset: once transit density reaches threshold levels and residential parking is decoupled from housing mandates, the pricing mechanism becomes unnecessary. The constraint is justified by the transition, not the steady state. Estimated sunset: 15-25 years as transit-oriented development matures.
constraint_indexing:constraint_classification(public_resource_pricing, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: COMMERCIAL DELIVERY SECTOR (TANGLED ROPE) — Benefits from predictable curb access and reduced double-parking enforcement, but pays permit fees that were previously externalized. Mixed experience: coordination function is real (loading zone availability improves), but extraction is also real (new cost burden).
constraint_indexing:constraint_classification(public_resource_pricing, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(local))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER (TANGLED ROPE) — From a civilizational perspective, cost-recovery pricing for scarce public infrastructure is a genuine coordination mechanism (solves the tragedy of the commons), but it is also extractive when applied to populations with inelastic demand and no viable alternatives. The constraint exhibits both functions simultaneously. Not a false summit — the coordination function is real, but so is the asymmetric burden.
constraint_indexing:constraint_classification(public_resource_pricing, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(public_resource_pricing_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(public_resource_pricing, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(public_resource_pricing, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

:- end_tests(public_resource_pricing_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.35): Moderate. The pricing mechanism extracts from car-dependent households (especially low-income) who have no viable alternative, but the extraction is not as severe as pure rent-seeking because the coordination function is real — curb space is genuinely scarce, and pricing does reduce cruising and fund alternative infrastructure. The value has increased over the interval (from 0.15 to 0.35) as permit fees have risen and enforcement has intensified. Suppression (0.45): Moderate. Significant barriers to exit include residential location lock-in, inadequate transit coverage in many neighborhoods, and the high cost of relocating closer to transit. But suppression is not total — some households can and do shift to transit or relocate. Suppression has increased over the interval as enforcement has become more systematic. Theater ratio (0.28): Low. The pricing mechanism is functionally effective at managing demand — it is not performative. Permit systems do reduce cruising time, do generate revenue, and do enable curb reallocation. The theater is limited to the equity-mitigation rhetoric (means-tested discounts that are too small to matter). Accessibility collapse (0.40): Moderate. Alternatives to car ownership exist in principle (transit, cycling, micromobility) but are not accessible to all populations — transit coverage is sparse in many neighborhoods, cycling is unsafe on many routes, and micromobility requires physical ability and digital access. The collapse is partial, not total. Resistance (0.65): High. The constraint meets substantial organized resistance from car-dependent constituencies, suburban commuter coalitions, and equity advocates. The resistance is not merely rhetorical — ballot initiatives, litigation, and political pressure have blocked or rolled back pricing schemes in multiple cities.
 *
 * PERSPECTIVAL GAP:
 *   This constraint demonstrates how a single structural arrangement produces radically different experiences depending on the agent's position. The municipal revenue authority sees pure coordination (Rope) — the pricing mechanism solves the allocation problem and funds public goods. The alternative mobility coalition also sees coordination (Rope) — pricing creates space for bike lanes and transit that were previously occupied by free car storage. The equity-focused reform coalition sees a transitional mechanism (Scaffold) — pricing is justified by the transition to higher transit density, not as a permanent steady state. The suburban commuter sees mixed coordination and extraction (Tangled Rope) — benefits from reduced cruising but pays substantially more. The low-income car-dependent household sees pure extraction (Snare) — excluded from a resource their tax dollars built, with no viable alternative. The analytical observer sees both functions simultaneously (Tangled Rope) — the coordination is real, but so is the asymmetric burden. The perspectival gap is not a measurement error — it is the structure of the constraint itself.
 *
 * DIRECTIONALITY LOGIC:
 *   Each perspective's directionality is derived from the agent's structural position. Low-income car-dependent households are victims with trapped exit options — they experience high directionality (d → 1.0) and thus high effective extraction. Suburban commuters are victims with constrained exit options — they experience moderate-high directionality. The municipal revenue authority is a beneficiary with arbitrage exit options — it experiences low or negative directionality (d → 0.0), meaning the constraint subsidizes rather than extracts from this agent. Alternative mobility users are beneficiaries with mobile exit options — they experience low directionality. Commercial delivery operators are mixed (both beneficiary and victim) with mobile exit options — they experience moderate directionality, reflecting the genuine coordination benefit alongside the new cost burden. The analytical observer sees both functions and does not experience extraction directly.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint resolves the mandatrophy by demonstrating that Tangled Rope is the correct classification at the analytical level: the coordination function is genuine (curb space is scarce, pricing does manage demand, revenue does fund alternatives), but the extraction is also genuine (low-income households are excluded, suburban commuters pay for previously free access, and the burden is asymmetric). The constraint is not a Rope misclassified as a Snare, nor a Snare misclassified as a Rope — it is genuinely both. The mandate (manage scarce public infrastructure efficiently) is live, but the mechanism also extracts asymmetrically. The Tangled Rope classification captures this dual function. The constraint would only be a Snare if the coordination function were cover (if curb space were not actually scarce, or if pricing did not actually manage demand, or if revenue did not actually fund alternatives). The constraint would only be a Rope if the extraction were negligible (if low-income households had viable alternatives, or if permit fees were trivial relative to income). Neither condition holds — both functions are real.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    transit_density_threshold,
    'At what transit service density does car ownership become genuinely optional rather than economically coerced?',
    'Empirical analysis of car ownership rates vs transit frequency/coverage; identification of threshold where ownership drops below 50% in comparable income cohorts',
    'If threshold is 15-minute headways on 3+ routes within 400m: many US cities are decades from making pricing non-extractive. If threshold is lower: pricing becomes coordination sooner.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(transit_density_threshold, empirical, 'Transit density threshold for non-coercive car ownership').

omega_variable(
    revenue_recycling_mechanism,
    'Does permit revenue actually fund alternative mobility infrastructure, or does it substitute for general fund allocations that would have occurred anyway?',
    'Budget analysis comparing transit/bike infrastructure spending in permit-revenue cities vs control cities; tracking of revenue flows through municipal accounts',
    'If revenue is truly additional: coordination function is stronger, beneficiary set expands. If revenue substitutes: extraction is higher, municipal authority is primary capturer.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(revenue_recycling_mechanism, empirical, 'Whether permit revenue is additional or substitutive').

omega_variable(
    income_elasticity_of_demand,
    'How does permit price sensitivity vary across income quintiles, and does the pricing structure account for this elasticity?',
    'Econometric analysis of permit take-up rates vs household income; comparison of flat-fee vs income-scaled permit structures',
    'If demand is highly inelastic for low-income households: pricing is more extractive than coordinating. If elastic: pricing successfully manages demand rather than excluding participants.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(income_elasticity_of_demand, empirical, 'Income-stratified price elasticity of curb space demand').

omega_variable(
    kernel_reading_ambiguity,
    'Is cost-recovery pricing the legitimate reading of curb space as scarce public infrastructure, or is it one contested reading among several (property tax entitlement, equity redistribution)?',
    'Cross-jurisdictional comparison of curb space governance models; analysis of which reading dominates in different political economies; identification of structural factors that select for each reading',
    'If cost-recovery is the only coherent reading: this constraint is a mountain (natural law of resource allocation). If multiple readings coexist: this constraint is one reading of a contested kernel, and the classification depends on which reading the observer holds.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_ambiguity, conceptual, 'Whether cost-recovery pricing is the unique legitimate reading or one among contested alternatives').

omega_variable(
    cs_framing_underdetermination,
    'Is the kernel ''curb space allocation rules'' (the formal permit system) or ''curb space legitimacy claims'' (the normative framework that justifies who gets to use public space)?',
    'Analysis of what authority structure adjudicates disputes: if formal rules dominate, kernel is the permit system; if legitimacy narratives dominate, kernel is the normative framework layered above the rules',
    'If kernel is formal rules: cs_pattern is likely formalized/extraction. If kernel is legitimacy claims: cs_pattern is likely distributed/practice, and the formal rules are downstream of contested norms.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(cs_framing_underdetermination, conceptual, 'Whether the kernel is the formal rule system or the legitimacy framework above it').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(public_resource_pricing, 0, 12).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(curb_pricing_tr_t0, public_resource_pricing, theater_ratio, 0, 0.2).
narrative_ontology:measurement(curb_pricing_tr_t3, public_resource_pricing, theater_ratio, 3, 0.22).
narrative_ontology:measurement(curb_pricing_tr_t6, public_resource_pricing, theater_ratio, 6, 0.25).
narrative_ontology:measurement(curb_pricing_tr_t9, public_resource_pricing, theater_ratio, 9, 0.27).
narrative_ontology:measurement(curb_pricing_tr_t12, public_resource_pricing, theater_ratio, 12, 0.28).

% Extraction over time
narrative_ontology:measurement(curb_pricing_be_t0, public_resource_pricing, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(curb_pricing_be_t3, public_resource_pricing, base_extractiveness, 3, 0.22).
narrative_ontology:measurement(curb_pricing_be_t6, public_resource_pricing, base_extractiveness, 6, 0.28).
narrative_ontology:measurement(curb_pricing_be_t9, public_resource_pricing, base_extractiveness, 9, 0.32).
narrative_ontology:measurement(curb_pricing_be_t12, public_resource_pricing, base_extractiveness, 12, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(curb_pricing_su_t0, public_resource_pricing, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(curb_pricing_su_t3, public_resource_pricing, suppression_requirement, 3, 0.35).
narrative_ontology:measurement(curb_pricing_su_t6, public_resource_pricing, suppression_requirement, 6, 0.4).
narrative_ontology:measurement(curb_pricing_su_t9, public_resource_pricing, suppression_requirement, 9, 0.43).
narrative_ontology:measurement(curb_pricing_su_t12, public_resource_pricing, suppression_requirement, 12, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(public_resource_pricing, resource_allocation).
narrative_ontology:affects_constraint(public_resource_pricing, residential_parking_mandates).
narrative_ontology:affects_constraint(public_resource_pricing, transit_service_frequency).
narrative_ontology:affects_constraint(public_resource_pricing, micromobility_infrastructure).

% DUAL FORMULATION NOTE:
% The curb space pricing constraint is part of a constraint family with three sibling readings (public_resource_pricing, property_tax_entitlement, equity_redistribution). Each reading instantiates a different constraint with different ε values. This story models the public_resource_pricing reading. The constraint is downstream of residential parking mandates (which create the scarcity) and upstream of transit service frequency and micromobility infrastructure (which are funded by permit revenue in this reading).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
