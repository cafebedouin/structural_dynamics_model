% ============================================================================
% CONSTRAINT STORY: technology_legitimacy_kernel__velocity_primacy_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-06-12
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_technology_legitimacy_kernel__velocity_primacy_reading, []).

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
 *   constraint_id: technology_legitimacy_kernel__velocity_primacy_reading
 *   human_readable: Technology Legitimacy by Velocity (Kernel Reading: Velocity Primacy)
 *   domain: energy_policy/climate_mitigation/technology_governance
 *
 * SUMMARY:
 *   This constraint instantiates the velocity-primacy reading of the
 *   technology-legitimacy kernel: a technology is legitimate for climate
 *   mitigation if and only if it can be deployed at scale within the
 *   remaining carbon budget timeline (2030/2050 targets). The reading treats
 *   deployment velocity as the PRIMARY legitimacy criterion. Under this
 *   frame, solar and wind technologies enter the beneficiary set (can be
 *   deployed rapidly at gigawatt scale within the 2030 window), while nuclear
 *   and other long-gestation technologies are marginalized or excluded (10–15
 *   year construction timelines exceed the velocity criterion). Grid
 *   operators and industrial heat users bear costs as the constraint mandates
 *   grid configurations and generation profiles optimized for rapid renewable
 *   deployment, often at the expense of dispatchability and baseload
 *   capacity. This is ONE reading of a contested kernel; sibling readings
 *   (reliability_primacy_reading, precautionary_reading) instantiate
 *   different legitimacy criteria from the same foundational commitment to
 *   climate mitigation. The engine computes per-seat classifications; this
 *   reading's authorized stakeholders may perceive the constraint differently
 *   depending on their position (velocity advocates vs. nuclear advocates vs.
 *   grid reliability maintainers).
 *
 * KEY AGENTS:
 *   - solar_wind_manufacturers: primary beneficiary (rapid deployment criterion favors their technologies)
 *   - utility_renewable_operators: primary beneficiary (velocity-optimized grid rules enable rapid solar/wind addition)
 *   - climate_advocacy_coalitions: secondary beneficiary (velocity criterion aligns with their political mobilization strategy)
 *   - nuclear_technology_developers: primary victim (long construction timelines disqualify them under velocity criterion)
 *   - grid_operators: victim (required to manage intermittency surge and grid stability under rapid renewable deployment)
 *   - industrial_heat_users: victim (velocity criterion deprioritizes reliable dispatchable heat generation, forcing end-use changes)
 *   - policymakers_and_regulators: agenda_setter (enforce velocity criterion through regulatory framework and investment priorities)
 *   - climate_scientists: observer (provide input on carbon budget adequacy but do not adjudicate legitimacy directly)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, 0.68).
domain_priors:suppression_score(technology_legitimacy_kernel__velocity_primacy_reading, 0.72).
domain_priors:theater_ratio(technology_legitimacy_kernel__velocity_primacy_reading, 0.41).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0.41).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, accessibility_collapse, 0.58).
narrative_ontology:constraint_metric(technology_legitimacy_kernel__velocity_primacy_reading, resistance, 0.69).

% --- Constraint claim ---
narrative_ontology:constraint_claim(technology_legitimacy_kernel__velocity_primacy_reading, tangled_rope).
narrative_ontology:human_readable(technology_legitimacy_kernel__velocity_primacy_reading, "Technology Legitimacy by Velocity (Kernel Reading: Velocity Primacy)").
narrative_ontology:topic_domain(technology_legitimacy_kernel__velocity_primacy_reading, "energy_policy/climate_mitigation/technology_governance").

domain_priors:requires_active_enforcement(technology_legitimacy_kernel__velocity_primacy_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(technology_legitimacy_kernel__velocity_primacy_reading, 'a6cb29a9-9244-4263-bcbb-c59d1790d6bc').
narrative_ontology:cs_kernel_codification('a6cb29a9-9244-4263-bcbb-c59d1790d6bc', distributed).
narrative_ontology:cs_authority_grounding('a6cb29a9-9244-4263-bcbb-c59d1790d6bc', distributed).
narrative_ontology:cs_reading_relation('a6cb29a9-9244-4263-bcbb-c59d1790d6bc', technology_legitimacy_kernel__reliability_primacy_reading, coexists_with).
narrative_ontology:cs_reading_relation('a6cb29a9-9244-4263-bcbb-c59d1790d6bc', technology_legitimacy_kernel__precautionary_reading, influences).
narrative_ontology:cs_axiom('a6cb29a9-9244-4263-bcbb-c59d1790d6bc', foundational, deployment_velocity_as_legitimacy_criterion).
narrative_ontology:cs_axiom_status(deployment_velocity_as_legitimacy_criterion, holdable).
narrative_ontology:cs_axiom_grounding('a6cb29a9-9244-4263-bcbb-c59d1790d6bc', deployment_velocity_as_legitimacy_criterion, empirically_contingent).
narrative_ontology:cs_axiom('a6cb29a9-9244-4263-bcbb-c59d1790d6bc', foundational, carbon_budget_determines_urgency_ordering).
narrative_ontology:cs_axiom_status(carbon_budget_determines_urgency_ordering, holdable).
narrative_ontology:cs_axiom_grounding('a6cb29a9-9244-4263-bcbb-c59d1790d6bc', carbon_budget_determines_urgency_ordering, empirically_contingent).
narrative_ontology:cs_reference_frame('a6cb29a9-9244-4263-bcbb-c59d1790d6bc', carbon_budget_bounded_climate_mitigation).
narrative_ontology:cs_drift_state('a6cb29a9-9244-4263-bcbb-c59d1790d6bc', contemporary_2026, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('a6cb29a9-9244-4263-bcbb-c59d1790d6bc', '').
narrative_ontology:cs_kernel_id(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, solar_wind_manufacturers).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, utility_renewable_operators).
narrative_ontology:constraint_beneficiary(technology_legitimacy_kernel__velocity_primacy_reading, climate_advocacy_coalitions).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_technology_developers).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, industrial_heat_users).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(technology_legitimacy_kernel__velocity_primacy_reading, utility_renewable_operators).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__velocity_primacy_reading, climate_urgency_doctrine).
narrative_ontology:constraint_vindicates(technology_legitimacy_kernel__velocity_primacy_reading, deployment_velocity_primacy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit directly from velocity-primacy framing: it legitimizes their technologies for rapid deployment and directs public and private capital toward their sector. Their manufacturing capacity expansion, supply chain buildout, and market growth are directly incentivized by the velocity criterion. They have exit options (can shift geographies, technology substrates, or deploy for non-climate applications) but the climate framing is their most powerful market anchor.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, solar_wind_manufacturers, beneficiary,
    powerful, generational, arbitrage, global).

% Benefit from velocity criterion through regulatory de-risking, power purchase agreement support, and grid priority rules. They are also constrained by mandates to rapidly integrate variable renewables, which creates operational costs (balancing, backup capacity, grid reconfiguration) they absorb as the cost of being the vehicle for velocity fulfillment. They cannot exit the constraint (are regulated utilities) but can lobby for rule changes.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, utility_renewable_operators, beneficiary,
    institutional, generational, constrained, global).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__velocity_primacy_reading, utility_renewable_operators, payer).

% Benefit from the velocity-primacy reading because it legitimizes the political mobilization they have organized around renewable energy deployment and climate urgency. Their coalition's identity, funding, and political power are constituted through the velocity framing. They can exit by shifting their advocacy framework, but doing so would dissolve their current organizing base.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, climate_advocacy_coalitions, beneficiary,
    organized, generational, mobile, global).

% Pay the constraint through regulatory exclusion: velocity criterion disqualifies their technologies (10–15 year construction timelines cannot meet 2030/2050 deployment windows). They have limited exit — nuclear physics cannot be accelerated by capital or engineering effort alone, and their technology is now deemed structurally 'slow' regardless of safety, emissions, or efficiency. They can develop smaller modular reactors or shift to non-climate markets (industrial heat, desalination), but the velocity frame erases their role in climate mitigation.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, nuclear_technology_developers, payer,
    powerful, generational, constrained, global).

% Bear costs of velocity requirement through mandated integration of highly variable renewable generation while holding responsibility for grid stability and reliability. They are rule-takers on velocity targets but have some agenda-setting power in how grids are reconfigured (technology mix for balancing, storage investment, demand response). Their exit options are limited (cannot refuse to operate the grid) but they can advocate for grid-code changes, storage investment priorities, or reliability-focused technology inclusion.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators, payer,
    institutional, biographical, constrained, regional).
narrative_ontology:stakeholder_secondary_role(technology_legitimacy_kernel__velocity_primacy_reading, grid_operators, agenda_setter).

% Need reliable, dispatchable heat for industrial processes (steel, cement, chemicals, food processing). Velocity criterion deprioritizes reliable dispatchable heat generation (natural gas, nuclear industrial heat, or geothermal baseload), forcing them either to electrify (at high cost and grid-load risk) or to shift their production (exit the region, outsource, or change product mix). They have low individual power but are organized through industrial associations.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, industrial_heat_users, payer,
    organized, biographical, constrained, regional).

% Enforce velocity criterion through regulatory frameworks (carbon budgets, renewable targets, grid codes, investment de-risking for solar/wind, technology assessments). They adjudicate legitimacy through policy and have agenda-setting power, but are themselves constrained by political coalitions (climate advocates, industry lobbying, constituent pressure). They can revise the velocity criterion by changing regulatory frameworks, but doing so requires overcoming the political organizations that have formed around it.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, policymakers_and_regulators, agenda_setter,
    institutional, biographical, constrained, national).

% Provide empirical inputs on carbon budgets, decarbonization pathways, and technology options but do not directly adjudicate which reading is legitimate. They model outcomes and highlight trade-offs; policy actors then choose which reading to institutionalize. They are excluded from the enforcement mechanism and hold no role in the beneficiary/payer structure, but their work shapes the contest's factual ground.
narrative_ontology:constraint_stakeholder(technology_legitimacy_kernel__velocity_primacy_reading, climate_scientists, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(technology_legitimacy_kernel__velocity_primacy_reading, solar_wind_manufacturers).
narrative_ontology:fixing_cost_class(technology_legitimacy_kernel__velocity_primacy_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Solves the collective-action problem of climate urgency: aligns technology deployment, investment, regulatory framework, and political mobilization around a single criterion (velocity). Enables rapid renewable capacity addition by removing uncertainty about which technologies are 'legitimate' for policy support, grid investment, and capital allocation. Coordinates expectations across manufacturers, utilities, and policymakers around deployment timelines.
% TRANSFER_FUNCTION: Transfers legitimacy, regulatory priority, and capital flow from slow-deployment technologies (nuclear, advanced geothermal, concentrated solar thermal) to fast-deployment technologies (solar PV, wind, battery storage). Transfers grid operational burden (balancing, stability management) from generation-side (the generators would bear it under a reliability-primary frame) to grid operators and demand-response systems. Transfers industrial heat sourcing burden from reliable dispatchable sources to intermittent renewables, forcing industrial users to electrify or shift production.
% ABSENT_VOICES: Nuclear technology workers and small modular reactor developers, who would argue that velocity is achievable with next-generation nuclear and that excluding nuclear is actually slowing decarbonization by forcing overreliance on variable renewables; grid reliability engineers, who would argue that the constraint's prioritization of velocity is creating systemic grid fragility that will require expensive mitigation; fossil fuel workers and transition advocates, who would argue that velocity demands are leaving workers and communities without just-transition pathways. These voices are structurally excluded from the velocity reading's authorization process because they contradict its framing.
% DISAPPEARANCE_RATIONALE: If the velocity-primacy reading disappeared overnight and were replaced by a framework that treated multiple legitimacy criteria equally (velocity, reliability, precaution in portfolio optimization), the technology landscape would reorganize rapidly: nuclear development would resume in high-capacity-need regions; grid operators would prioritize storage and baseload alongside renewables; industrial heat would access reliable dispatchable generation; renewable deployment would decelerate from maximum-speed to sustainable-speed; investment would shift from pure capacity to resilience and grid balancing. The world does not rearrange back to pre-constraint state (climate urgency remains), but it rearranges away from velocity-dominated technology choices.
% FOUNDING_PROBLEM: Climate urgency: the remaining global carbon budget for 1.5°C warming is finite (roughly 400 gigatons CO2 as of 2023) and nearly all pathways to stay under 1.5°C require rapid decarbonization, with most emissions reductions occurring before 2030. Existing technology stock is slow to turn over, and new infrastructure takes years to build. The founding problem is: how do we decarbonize fast enough to stay within the carbon budget?
% FOUNDING_PROBLEM_CORROBORATION: IPCC Special Report on 1.5°C (Masson-Delmotte et al., 2018), independent research by Stanford, MIT, and other non-advocate institutions, climate finance analysts, and grid operators themselves all atttest that rapid deployment of renewable capacity is necessary for 1.5°C pathways. Even fossil fuel utilities and nuclear advocates concede the founding problem exists (they differ on the solution). The founding problem is corroborated by parties outside the velocity-advocate coalition (climate scientists, grid operators, some industrial users acknowledge the urgency).
narrative_ontology:disappearance_verdict(technology_legitimacy_kernel__velocity_primacy_reading, world_rearranges).
narrative_ontology:founding_problem_status(technology_legitimacy_kernel__velocity_primacy_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 'e03e2210ef39e1af4d109acadf9515e5d2d8b7d7',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-22',
    'no_scope_rebuild_haiku3', 'agent/example_platform_commission.json',
    'claude-haiku-4-5-20251001', 'max_tokens=16384,thinking=disabled,temperature=api_default').
narrative_ontology:story_seed(technology_legitimacy_kernel__velocity_primacy_reading, 'none', 1).
narrative_ontology:epsilon_provenance(technology_legitimacy_kernel__velocity_primacy_reading, 0.68, 'claude-haiku-4-5-20251001', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(technology_legitimacy_kernel__velocity_primacy_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(technology_legitimacy_kernel__velocity_primacy_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness measures how much the velocity-primacy reading extracts from non-velocity technologies and their stakeholders. At t=0, extractiveness is moderate (0.42) because the reading is still primarily advocated rather than regulatory; by t=25, it reaches 0.68 as policy institutionalizes velocity as the legitimacy standard and investment flows shift decisively toward fast-deployment renewables. Suppression rises even faster (from 0.48 to 0.72) because maintaining the velocity criterion requires active regulatory measures: restricting new nuclear permits, mandating grid codes that favor variable renewables, de-risking solar/wind investment while raising capital costs for nuclear. Theater ratio rises more slowly (0.18 to 0.41) because the legitimacy framing itself is genuine — velocity IS a real consideration within the carbon budget — but grows as the ratio of performative velocity rhetoric (grid 'transition' theater, symbolic renewable capacity announcements) to actual emissions reduction grows. The measurement series tracks institutionalization: velocity begins as an advocacy position within the climate movement, solidifies into policy at mid-interval (t=10–15), and persists as the regulatory baseline at t=25 despite growing evidence of grid stress and industrial heat shortfalls (theater ratio uptick). Suppression requirement grows fastest because the velocity criterion has no natural constituency to maintain it; enforcement must actively overcome resistance from nuclear advocates, grid operators, and industrial users.
 *
 * PERSPECTIVAL GAP:
 *   The velocity-primacy reading should compute differently across seats. From the solar/wind manufacturer seat: the constraint is a genuine coordination mechanism (ensuring that climate urgency, deployment speed, and renewable capability align). From the nuclear developer seat: the constraint is pure extraction (they are excluded from legitimacy by a criterion they cannot meet through any operational efficiency, only through physics change). From the grid operator seat: the constraint is asymmetric extraction disguised as coordination (they are mandated to integrate highly variable generation while being told reliability is their responsibility, not the generation's — a coordination function for some, an extraction mechanism for others). From the climate-science seat: velocity may be a necessary but insufficient condition for adequate decarbonization, and the reading's exclusion of reliable technologies may actually harm total emissions reduction if it drives grid configurations that require fossil fuel peaking. The engine computes these divergences from the structural data; the claim and metrics are authored independently so the divergence itself is measurable.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality derives from beneficiary/victim structure and exit options. Solar/wind manufacturers have high d toward 0.0 (beneficiary, high exit via global markets); nuclear developers have high d toward 1.0 (victim, trapped by the regulatory criterion they cannot engineering-solve). Grid operators sit at d~0.65 (payers, but with some agency in how they operate — constrained, not trapped; they can advocate for rule changes or shift their technology portfolio, but the velocity criterion makes that costly). Industrial heat users sit at d~0.72 (high d: they need reliable heat, the velocity criterion deprioritizes reliable dispatchable sources, and they have limited exit — reengineering to renewables-only is slow and costly). Climate advocacy coalitions sit at d~0.15 (beneficiary: the reading vindicates their urgency frame and mobilizes investment in their preferred technologies). No overrides are necessary; the derived directionality reflects the structural asymmetry.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (climate urgency within a remaining carbon budget) is LIVE and widely attested outside the beneficiary set; IPCC reports, G20 commitments, and independent climate modeling all confirm the carbon budget constraint. However, mandatrophy appears in the constraint's evolution: the founding problem is framed NARROWLY in this reading as 'velocity is the solution,' when the actual founding problem is 'how do we decarbonize adequately within the carbon budget?' The velocity reading assumes that fast deployment of renewables IS decarbonization adequacy, but grid physics and industrial heat requirements complicate that assumption. If the founding problem (adequate decarbonization) diverges from the constraint's solution (velocity maximization), mandatrophy begins to set in. The measurement series captures this: extractiveness plateaus at 0.68 while theater ratio continues rising, suggesting the reading's enforcement increasingly depends on its framing power rather than its functional coupling to actual emissions reduction. This is the pre-mandatrophy state: the constraint is still enforced and believed in, but its solution-function has begun to decouple from the problem-function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    velocity_vs_decarbonization_adequacy,
    'Does the 2030/2050 carbon budget timeline require velocity-prioritized deployment, or does it require a technology mix that balances velocity, reliability, and emissions reductions across multiple vectors simultaneously?',
    'Integrated Assessment Models (IAMs) from independent climate research bodies (not beneficiary-aligned institutes) that model decarbonization pathways: does the velocity-dominant scenario meet temperature targets better than technology-balanced scenarios, or does omitting slower-to-deploy but high-capacity technologies (nuclear, grid-scale storage) require deployment acceleration of renewable capacity beyond technical/material feasibility?',
    'If velocity dominance produces adequate decarbonization: the reading''s core premise is vindicated and the constraint stands. If balanced technology mix produces better outcomes with lower physical strain: the reading''s distinction collapses and the kernel''s referent shifts to adequacy, not velocity — reclassifies to reliability_primacy_reading or a third reading emphasizing portfolio optimization.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(velocity_vs_decarbonization_adequacy, empirical, 'Whether deployment velocity is a sufficient condition for climate legitimacy or a necessary but insufficient condition within a bounded carbon budget.').

omega_variable(
    kernel_reading_authority_grounding,
    'Who adjudicates what counts as ''legitimate'' technology for climate mitigation — the reading''s own epistemic foundation — and does that authority derive from climate physics, policy urgency, or political organization of renewable technology advocates?',
    'Examine the origin of the 2030/2050 targets: are they physics-derived (maximum emissions for temperature targets), policy-negotiated (political feasibility of national commitments), or advocate-driven (what renewable-allied actors can mobilize capital for). Trace the authority chain: IPCC → national climate frameworks → investment decision-makers → technology deployment.',
    'If authority is physics-derived, the reading is expert-grounded and claims legitimacy from empirical climate modeling. If policy-negotiated or advocate-driven, the reading''s legitimacy is distributed among negotiating parties (coexists_with sibling readings) rather than foreclosing competitors. Shifts cs_structure.authority_grounding from expertise to distributed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_authority_grounding, conceptual, 'Whether the velocity-primacy reading''s authority grounding is epistemic (climate science) or organizational (advocate coalition power).').

omega_variable(
    suppression_mechanism_internalization,
    'Is the measured suppression (0.72) of non-velocity technologies structural (regulatory exclusion, investment de-risking favoring renewables, grid code changes mandating certain generation profiles) or internalized (nuclear and industrial heat operators internalize the velocity framing as the legitimate standard and deprioritize their own deployment)?',
    'Post-policy measurement: if velocity-framing regulatory infrastructure is removed and nuclear deployment accelerates rapidly while solar/wind investment plateaus, suppression was primarily structural. If nuclear remains deprioritized and solar/wind remain favored despite regulatory neutrality, suppression is internalized — the reading has constituted the industry''s self-concept.',
    'If structural: suppression is a raw policy instrument the next administration could reverse quickly, and the constraint''s persistence is enforcement-dependent. If internalized: the technology sectors have adopted the reading as their operational frame, and reversing it requires shifting industry identity and investment practices — higher exit cost for the suppressed parties, higher persistence of the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_internalization, empirical, 'Whether suppression of non-velocity technologies is policy-structural or industry-internalized.').

omega_variable(
    velocity_reading_vs_sibling_foreclosure,
    'Does the velocity-primacy reading''s core axiom (deployment_velocity_as_legitimacy_criterion) logically foreclose the reliability_primacy_reading and precautionary_reading, or do the readings coexist as different weightings of legitimate concerns that can be held simultaneously by different parties?',
    'Examine whether a single framework can hold ''velocity is necessary'' AND ''reliability is necessary'' AND ''precaution is necessary'' without contradiction, or whether one reading''s premise directly contradicts another''s. If the climate budget is truly binding and immutable (physics-derived), does that make alternative technology criteria illegitimate or merely secondary? Can a party hold all three readings simultaneously (velocity + reliability + precaution in weighted portfolio), or does accepting velocity as the PRIMARY legitimacy criterion require rejecting the others'' PRIMARY claims?',
    'If readings can coexist: they are genuinely distributed (different parties hold different weightings, all defensible); the engine rates as coexists_with. If velocity acceptance forecloses reliability/precaution: the reading is exclusive; the engine rates as forecloses. This determines whether the kernel contest is a settled competition or an ongoing multivalent negotiation.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(velocity_reading_vs_sibling_foreclosure, conceptual, 'Whether velocity-primacy logically forecloses sibling readings or coexists with them.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(technology_legitimacy_kernel__velocity_primacy_reading, 0, 25).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(tech_tr_t0, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 0, 0.18).
narrative_ontology:measurement_basis(tech_tr_t0, observed).
narrative_ontology:measurement(tech_tr_t5, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 5, 0.24).
narrative_ontology:measurement_basis(tech_tr_t5, observed).
narrative_ontology:measurement(tech_tr_t10, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 10, 0.31).
narrative_ontology:measurement_basis(tech_tr_t10, observed).
narrative_ontology:measurement(tech_tr_t15, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 15, 0.37).
narrative_ontology:measurement_basis(tech_tr_t15, observed).
narrative_ontology:measurement(tech_tr_t20, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 20, 0.4).
narrative_ontology:measurement_basis(tech_tr_t20, projected).
narrative_ontology:measurement(tech_tr_t25, technology_legitimacy_kernel__velocity_primacy_reading, theater_ratio, 25, 0.41).
narrative_ontology:measurement_basis(tech_tr_t25, projected).

% Extraction over time
narrative_ontology:measurement(tech_be_t0, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 0, 0.42).
narrative_ontology:measurement_basis(tech_be_t0, observed).
narrative_ontology:measurement(tech_be_t5, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 5, 0.51).
narrative_ontology:measurement_basis(tech_be_t5, observed).
narrative_ontology:measurement(tech_be_t10, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 10, 0.58).
narrative_ontology:measurement_basis(tech_be_t10, observed).
narrative_ontology:measurement(tech_be_t15, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 15, 0.63).
narrative_ontology:measurement_basis(tech_be_t15, observed).
narrative_ontology:measurement(tech_be_t20, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 20, 0.66).
narrative_ontology:measurement_basis(tech_be_t20, projected).
narrative_ontology:measurement(tech_be_t25, technology_legitimacy_kernel__velocity_primacy_reading, base_extractiveness, 25, 0.68).
narrative_ontology:measurement_basis(tech_be_t25, projected).

% Suppression requirement over time
narrative_ontology:measurement(tech_su_t0, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 0, 0.48).
narrative_ontology:measurement_basis(tech_su_t0, observed).
narrative_ontology:measurement(tech_su_t5, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 5, 0.57).
narrative_ontology:measurement_basis(tech_su_t5, observed).
narrative_ontology:measurement(tech_su_t10, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 10, 0.64).
narrative_ontology:measurement_basis(tech_su_t10, observed).
narrative_ontology:measurement(tech_su_t15, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 15, 0.69).
narrative_ontology:measurement_basis(tech_su_t15, observed).
narrative_ontology:measurement(tech_su_t20, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 20, 0.71).
narrative_ontology:measurement_basis(tech_su_t20, projected).
narrative_ontology:measurement(tech_su_t25, technology_legitimacy_kernel__velocity_primacy_reading, suppression_requirement, 25, 0.72).
narrative_ontology:measurement_basis(tech_su_t25, projected).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(technology_legitimacy_kernel__velocity_primacy_reading, resource_allocation).
narrative_ontology:boltzmann_floor_override(technology_legitimacy_kernel__velocity_primacy_reading, 0.18).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel__reliability_primacy_reading).
narrative_ontology:affects_constraint(technology_legitimacy_kernel__velocity_primacy_reading, technology_legitimacy_kernel__precautionary_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading (velocity_primacy) of the contested technology_legitimacy_kernel. Sibling readings instantiate different technology legitimacy criteria from the same foundational commitment to climate mitigation. The three readings differ in their beneficiary/victim structures, measured extractiveness, and stakeholder directionality. They are linked via network.affects_constraints because acceptance of one reading creates structural pressure on the others (e.g., velocity dominance reduces capital availability for slow-deployment alternatives, which influences the reliability and precautionary readings' operating environment). The kernel contest is live; no single reading forecloses the others logically, but they do influence each other through policy, capital, and institutional mechanisms.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(technology_legitimacy_kernel__velocity_primacy_reading, institutional, 0.58).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
