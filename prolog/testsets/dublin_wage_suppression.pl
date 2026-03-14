% ============================================================================
% CONSTRAINT STORY: dublin_wage_suppression
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_dublin_wage_suppression, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: dublin_wage_suppression
 *   human_readable: Dublin Wage Suppression via Rental Extraction
 *   domain: economic/labor/housing
 *
 * SUMMARY:
 *   Dublin's wage suppression operates through a tangled
 *   coordination-extraction hybrid: housing supply constraints create
 *   artificial scarcity rents that consume worker income faster than nominal
 *   wages can rise, while planning restrictions and development incentive
 *   structures ostensibly coordinate urban growth but actually concentrate
 *   wealth extraction in the property sector. Workers face suppressed real
 *   wages (nominal increases consumed by rental inflation), landlords and
 *   developers benefit from scarcity rents and land appreciation, and policy
 *   authorities maintain planning theaters that produce the appearance of
 *   growth management while enabling extraction. The constraint is neither
 *   pure coordination (genuine housing-supply problem exists) nor pure
 *   extraction (some actors genuinely benefit from coordinated development
 *   frameworks), but rather a hybrid where coordination mechanisms have been
 *   captured and repurposed for asymmetric extraction. Extractiveness has
 *   risen from 0.38 to 0.58 over the measurement interval as rental inflation
 *   accelerated faster than wage growth.
 *
 * KEY AGENTS:
 *   - Dublin Service Sector Workers: Primary victims (powerless/trapped) — hospitality, retail, care workers earning €25k-35k/year while median rent is €1,600/month; geographic immobility due to sector location requirements
 *   - Property Owners and Developers: Primary beneficiaries (institutional/arbitrage) — capture appreciation and rental extraction; benefit from planning frameworks that limit competition
 *   - Dublin City Council and Planning Authority: Secondary institutional actor (institutional/arbitrage) — maintains planning restrictions; sees coordination function but enables extraction through scarcity
 *   - Organized Labor Unions: Secondary actors (organized/constrained) — achieve nominal wage gains that are immediately consumed by rental inflation; constrained by sector fragmentation
 *   - Community Housing Advocates: Organized challengers (organized/constrained) — building alternative coordination models (co-housing, CLTs, social housing) but face extraction through market pressure and underinvestment
 *   - Policy Reform Coalition: Institutional reformers (organized/constrained) — see scaffold exit path through rent controls, vacant property levies, rapid social housing; represent genuine alternative coordination
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(dublin_wage_suppression, 0.58).
domain_priors:suppression_score(dublin_wage_suppression, 0.68).
domain_priors:theater_ratio(dublin_wage_suppression, 0.52).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(dublin_wage_suppression, extractiveness, 0.58).
narrative_ontology:constraint_metric(dublin_wage_suppression, suppression_requirement, 0.68).
narrative_ontology:constraint_metric(dublin_wage_suppression, theater_ratio, 0.52).

% --- Constraint claim ---
narrative_ontology:constraint_claim(dublin_wage_suppression, tangled_rope).
narrative_ontology:human_readable(dublin_wage_suppression, "Dublin Wage Suppression via Rental Extraction").
narrative_ontology:topic_domain(dublin_wage_suppression, "economic/labor/housing").

domain_priors:requires_active_enforcement(dublin_wage_suppression).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(dublin_wage_suppression, landlord_class).
narrative_ontology:constraint_beneficiary(dublin_wage_suppression, real_estate_investors).
narrative_ontology:constraint_beneficiary(dublin_wage_suppression, property_development_sector).
narrative_ontology:constraint_victim(dublin_wage_suppression, dublin_workers).
narrative_ontology:constraint_victim(dublin_wage_suppression, service_sector_employees).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: DUBLIN SERVICE WORKER (SNARE) — Trapped in Dublin labor market by employment location; faces extraction through high rental costs that consume 45-60% of wages. No real exit: geographic relocation breaks career/social networks; remote work unavailable in hospitality/retail sectors. Suppression operates through housing scarcity and planning restrictions that artificially limit supply. Maximum experienced extraction — zero degrees of freedom.
constraint_indexing:constraint_classification(dublin_wage_suppression, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: ORGANIZED LABOR (TANGLED ROPE) — Constrained by sector fragmentation and geographic mobility costs. Benefits from labor coordination mechanisms and some wage negotiation leverage in tourism/hospitality. But wage gains are systematically extracted through rental inflation — nominal wage increases translate to zero real income growth. Mixed experience: some coordination function (collective bargaining) alongside asymmetric extraction (housing rents rising faster than negotiated wages).
constraint_indexing:constraint_classification(dublin_wage_suppression, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 3: PROPERTY DEVELOPER (ROPE) — Experiences the constraint as pure coordination: planning frameworks, housing permits, and development incentives solve the genuine collective problem of housing supply. But the coordination is asymmetric — developers capture long-term value extraction through scarcity rents and land appreciation. Net beneficiary with low experienced extraction (they are extractors, not targets).
constraint_indexing:constraint_classification(dublin_wage_suppression, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: DUBLIN CITY COUNCIL (PITON) — Maintains planning restrictions ostensibly to preserve character and manage growth, but these same restrictions artificially constrain housing supply and drive rents up. The planning ritual persists through institutional inertia; the original coordination function (controlled urban development) has atrophied while the extraction mechanism (scarcity rents) remains. Theater ratio reflects performative consultation processes that lack real power to expand housing stock.
constraint_indexing:constraint_classification(dublin_wage_suppression, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(regional))).

% PERSPECTIVE 5: COMMUNITY HOUSING ADVOCATES (TANGLED ROPE) — Organized agents (housing charities, tenant unions, policy groups) see genuine coordination function: co-housing models, community land trusts, and social housing coordinate resource allocation. But these alternatives face extraction through underinvestment, political marginalization, and regulatory friction. The constraint contains both a coordination mechanism they're trying to build (affordable housing) and the extraction mechanism that suppresses it (market rents outpacing community options).
constraint_indexing:constraint_classification(dublin_wage_suppression, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 6: HOUSING FIRST / POLICY REFORMERS (SCAFFOLD) — See the wage suppression as a temporary institutional problem with an exit path: rent controls, property taxes on non-primary residences, vacant property levies, and rapid social housing deployment can bypass the extraction mechanism within 10-15 year window. Low effective extraction because this coalition perceives and is acting on a sunset clause — if reforms succeed, the constraint's extraction power collapses. Theater ratio lower here because policy intervention is outcome-oriented rather than performative.
constraint_indexing:constraint_classification(dublin_wage_suppression, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 7: ANALYTICAL OBSERVER / MARKET NATURAL LAW (MOUNTAIN) — From a civilizational perspective, the constraint could be read as an immutable natural law: cities generate scarcity rents; land supply is finite; property values rise with population density. However, this naturalizes contingent institutional arrangements (planning restrictions, zoning regulations, investment tax policy, foreign capital inflows) as laws of physics. The engine's false summit detector will flag this — wage suppression in Dublin is not natural law but political economy.
constraint_indexing:constraint_classification(dublin_wage_suppression, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(dublin_wage_suppression_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(dublin_wage_suppression, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(dublin_wage_suppression, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(dublin_wage_suppression, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(dublin_wage_suppression, TR),
    TR >= 0.70.

:- end_tests(dublin_wage_suppression_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high. The constraint extracts through rental inflation that outpaces wage growth; this extraction is material and measurable. The value reflects that this is not total extraction (workers can survive, some save, some negotiate modest raises) but substantial enough that real income stagnates despite nominal wage increases. The upward trajectory (0.38→0.58) reflects accelerating Dublin rents outpacing service-sector wage growth. Suppression (0.68): High. Multiple reinforcing barriers prevent exit: sector workers face geographic immobility (hospitality jobs are location-specific), career switching costs, social network ties to Dublin, and the aspiration to own property (identity-lock component). Planning restrictions artificially limit housing supply, making relocation to cheaper cities also difficult (same scarcity-rent dynamics replicate across Irish regional capitals). Theater ratio (0.52): Moderate. Planning consultations and development boards create performative appearance of coordinated growth management, but actual decisions track investment incentives and existing-owner preferences. The theater is not high (actual planning does occur, some projects are blocked, some limits are enforced) but sufficient to obscure the extraction mechanism.
 *
 * PERSPECTIVAL GAP:
 *   The gap between worker and developer perspectives is maximal: 0.95 d-value (snare) vs 0.15 d-value (rope). Both experience the same constraint (Dublin housing scarcity, planning restrictions, rental costs) but through completely different structural relationships. The worker's real income stagnates; the developer's wealth appreciates. The worker cannot exit; the developer can exit or arbitrage. The planning authority's perspective reveals the piton mechanism: the planning apparatus maintains a coordination fiction (managing growth) while enabling extraction (concentrating land appreciation). The reformer's scaffold perspective hinges on the omega variable about supply elasticity — if housing can be deployed quickly post-reform, the sunset is real; if not, the scaffold is aspirational theater. The mountain perspective represents the risk of naturalizing political economy (scarcity rents, planning capture, land value concentration) as natural laws of urban economics.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality flows from structural position. Workers are primary targets (beneficiary/victim declarations: victim status; exit_options trapped). Landlords/developers are primary beneficiaries (beneficiary status; exit_options arbitrage → low d → negative effective extraction). Organized labor occupies middle position (victim status due to wage suppression; exit_options constrained → moderate d → moderate f(d)). Planning authorities are secondary beneficiaries in their institutional capacity (arbitrage via rent extraction to connected parties; though individual planners may not benefit). The derivation produces high d for trapped agents, low d for institutional beneficiaries, and moderate d for constrained agents. The directionality_overrides array is empty because the structural derivation captures the true relationships: workers bear extraction via high d, beneficiaries experience low/negative extraction via low d.
 *
 * MANDATROPHY ANALYSIS:
 *   Dublin wage suppression resolves mandatrophy by clarifying that tangled rope classification captures what pure classification types would miss: this is neither 'just coordination' (planning frameworks do address real housing-supply issues) nor 'just extraction' (the rental extraction is enabled by a coordination mechanism, not independent of it). The mandatrophy resolution shows that the tangled rope is the correct type because: (1) genuine coordination function exists (planning manages density, preserves character, coordinates infrastructure); (2) asymmetric extraction exists (landlords/developers capture appreciation; workers bear scarcity costs); (3) active enforcement is required (planning permission blocks alternatives; zoning prevents development; tax incentives favor investment property). The piton perspective on planning authorities is diagnostically important: it reveals that enforcement itself has become performative — planning rituals produce the appearance of coordination while the extraction mechanism remains. The scaffold perspective identifies the key mandatrophy resolution: if Housing First policies succeed, the constraint's extractiveness collapses because the artificial scarcity that enables both the coordination fiction and the extraction mechanism is removed. The mountain perspective is correctly flagged as false summit — market scarcity is not natural law but political economy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    planning_restriction_justification,
    'How much of Dublin''s housing scarcity reflects genuine preservation needs vs. regulatory capture by existing property owners?',
    'Comparative analysis: cities with similar architectural/heritage value but different planning regimes; cross-national density/rent correlations; analysis of planning objection source data',
    'If genuine: constraint may shift toward rope (coordination problem). If regulatory capture: constraint remains snare/tangled_rope with high extraction. Determines whether planning reform is feasible.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(planning_restriction_justification, empirical, 'Whether planning restrictions serve heritage preservation or property owner capture').

omega_variable(
    wage_elasticity_to_housing_cost,
    'Are Dublin nominal wages depressed *because* of high housing costs, or do high housing costs simply consume pre-existing wage levels?',
    'Time-series analysis of wage growth vs rent growth; comparison with equivalent workers in lower-rent cities; survey of employer wage-setting rationales',
    'If causal (high rents suppress wages): extraction is multiplicative — workers lose both to rent and to lower nominal wages. If consumption (wages are independent; rents consume them): extraction is additive — rental extraction only.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(wage_elasticity_to_housing_cost, empirical, 'Causal direction between rental costs and wage suppression').

omega_variable(
    foreign_capital_role,
    'What proportion of Dublin property ownership is foreign/non-resident capital vs. domestic landlords? Does this affect extraction mechanism?',
    'Ownership registry analysis; corporate structure tracing; comparison of rent extraction rates by owner type; capital flow tracing',
    'If predominantly foreign: extraction is capital flight (rents leave Ireland). If domestic: extraction is wealth redistribution within local system. If mixed: determines dual pathology (labor exploitation + capital flight).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(foreign_capital_role, empirical, 'Proportion and role of foreign capital in Dublin property extraction').

omega_variable(
    identity_lock_housing_aspiration,
    'To what extent does housing aspiration (owner-occupancy as identity goal) bind workers to accepting wage suppression?',
    'Survey: what percentage of Dublin workers see home ownership as central to identity/social standing? Compare with cities where renting is de-stigmatized. Measure willingness to relocate if renting is viewed as temporary vs. permanent.',
    'If high identity fusion with ownership: workers accept suppressed wages to save for down payments; identity lock is additional suppression mechanism. If low: workers more mobile and more likely to exit. Determines whether reform must address identity framing or just material barriers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_housing_aspiration, empirical, 'Identity fusion between homeownership aspiration and wage suppression acceptance').

omega_variable(
    supply_elasticity_timing,
    'How long would it take for housing supply to respond to price signals if planning restrictions were removed? What is the realistic sunset timeline?',
    'Construction industry capacity analysis; permitting timelines; land assembly costs; comparison with rapid housing deployments in other cities (Seoul, Singapore, Vienna)',
    'If short timeline (3-5 years): scaffold perspective is accurate, sunset clause is real. If long timeline (15+ years): scaffold is aspirational, extraction persists across generational horizon.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(supply_elasticity_timing, empirical, 'Supply response timeline and realism of housing-reform sunset clause').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(dublin_wage_suppression, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(dws_tr_t0, dublin_wage_suppression, theater_ratio, 0, 0.42).
narrative_ontology:measurement(dws_tr_t5, dublin_wage_suppression, theater_ratio, 5, 0.48).
narrative_ontology:measurement(dws_tr_t10, dublin_wage_suppression, theater_ratio, 10, 0.52).

% Extraction over time
narrative_ontology:measurement(dws_be_t0, dublin_wage_suppression, base_extractiveness, 0, 0.38).
narrative_ontology:measurement(dws_be_t5, dublin_wage_suppression, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(dws_be_t10, dublin_wage_suppression, base_extractiveness, 10, 0.58).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(dublin_wage_suppression, resource_allocation).
narrative_ontology:boltzmann_floor_override(dublin_wage_suppression, 0.18).
narrative_ontology:affects_constraint(dublin_wage_suppression, irish_housing_affordability_crisis).
narrative_ontology:affects_constraint(dublin_wage_suppression, service_sector_wage_stagnation_eu).
narrative_ontology:affects_constraint(dublin_wage_suppression, planning_regulatory_capture).

% DUAL FORMULATION NOTE:
% Dublin wage suppression decomposes into three structurally linked constraints: (1) housing_scarcity_extraction (ε=0.62, snare from worker perspective), (2) planning_regulatory_capture (ε=0.55, piton), (3) investment_property_subsidies (ε=0.48, rope from developer perspective). Each has distinct ε and metrics but they are coupled through the housing market. This story represents the integrated system; downstream constraints represent structural components.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
