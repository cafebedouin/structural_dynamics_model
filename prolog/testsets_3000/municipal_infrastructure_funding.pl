% ============================================================================
% CONSTRAINT STORY: municipal_infrastructure_funding
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_municipal_infrastructure_funding, []).

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
    constraint_indexing:directionality_override/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: municipal_infrastructure_funding
 *   human_readable: Municipal Infrastructure Funding Constraint
 *   domain: public_finance/urban_governance
 *
 * SUMMARY:
 *   Municipal infrastructure funding represents a structural constraint that
 *   exhibits the full spectrum of DR classification types. The constraint
 *   emerges from a genuine coordination problem: cities need capital for
 *   large-scale infrastructure projects (water systems, roads, transit) that
 *   no single actor can fund independently. However, the institutional
 *   solutions to this coordination problem (municipal debt markets,
 *   state/federal grants, regulatory frameworks) have become extraction
 *   mechanisms in their own right. Low-income residents bear the costs of
 *   deferred maintenance (trapped in municipalities with degrading services).
 *   The bond market and state/federal administrators capture benefits
 *   (through interest spread, covenant requirements, policy control). The
 *   essential service systems themselves cannot exit or organize and suffer
 *   maximal extraction through deferred maintenance cycles. The constraint
 *   demonstrates how coordination mechanisms transform into extraction when
 *   power asymmetries are embedded into the structural solution.
 *
 * KEY AGENTS:
 *   - Low-income residents: Primary victims (powerless/trapped) — confined to municipalities with deteriorating services, bear costs of deferred maintenance through health impacts and property decline
 *   - Middle-income homeowners: Secondary victims (moderate/constrained) — own property in municipalities, face both genuine coordination benefits and asymmetric extraction costs
 *   - State/federal grant administrators: Primary beneficiaries (institutional/arbitrage) — control funding allocation, maintain policy flexibility, benefit from system stability
 *   - Municipal bond market: Powerful beneficiary (powerful/mobile) — captures interest spread and concessions; can reallocate capital to other sectors
 *   - Essential service infrastructure systems: Victims (powerless/trapped) — cannot organize or exit, bear full deferred maintenance costs across generations
 *   - Municipal finance regulatory framework: Institutional actor (institutional/constrained) — maintains balanced budget requirements and debt covenants; sees own mechanisms as degraded (piton perspective)
 *   - Infrastructure renewal coalition: Organized agents (organized/constrained) — mayors, unions, advocacy groups building alternative funding pathways with sunset logic
 *   - Analytical observer: Civilizational observer (analytical/analytical) — risks naturalizing contingent funding institutional arrangements as immutable fiscal laws
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(municipal_infrastructure_funding, 0.52).
domain_priors:suppression_score(municipal_infrastructure_funding, 0.65).
domain_priors:theater_ratio(municipal_infrastructure_funding, 0.58).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(municipal_infrastructure_funding, extractiveness, 0.52).
narrative_ontology:constraint_metric(municipal_infrastructure_funding, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(municipal_infrastructure_funding, theater_ratio, 0.58).

% --- Constraint claim ---
narrative_ontology:constraint_claim(municipal_infrastructure_funding, tangled_rope).
narrative_ontology:human_readable(municipal_infrastructure_funding, "Municipal Infrastructure Funding Constraint").
narrative_ontology:topic_domain(municipal_infrastructure_funding, "public_finance/urban_governance").

domain_priors:requires_active_enforcement(municipal_infrastructure_funding).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(municipal_infrastructure_funding, senior_debt_holders).
narrative_ontology:constraint_beneficiary(municipal_infrastructure_funding, state_and_federal_grant_administrators).
narrative_ontology:constraint_victim(municipal_infrastructure_funding, low_income_residents).
narrative_ontology:constraint_victim(municipal_infrastructure_funding, essential_service_provision).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: LOW-INCOME RESIDENT (SNARE) — Trapped in municipality with deteriorating water systems, roads, and public services. Cannot exit without relocation cost (not available). Bears full cost of deferred maintenance through service degradation, health impacts, and property value decline. Extraction operates through structural immobility.
constraint_indexing:constraint_classification(municipal_infrastructure_funding, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: MIDDLE-INCOME HOMEOWNER (TANGLED ROPE) — Constrained by property ties and relocation costs but can exit if conditions degrade sufficiently. Genuine coordination function exists: infrastructure maintenance benefits all residents. But extraction is asymmetric — debt service and privatization deals concentrate benefits to creditors while spreading costs across homeowners. Both coordination (everyone benefits from maintained infrastructure) and extraction (costs borne by residents, benefits captured by debt holders) are structurally present.
constraint_indexing:constraint_classification(municipal_infrastructure_funding, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

% PERSPECTIVE 3: STATE/FEDERAL GRANT ADMINISTRATOR (ROPE) — Experiences constraint as pure coordination mechanism: federal infrastructure grants fund municipalities, enabling collective action on problems no single entity can solve. High arbitrage capacity (can reallocate funding, adjust requirements). Benefits from maintaining the grant system as a functional tool. Minimal experienced extraction — the constraint solves a genuine coordination problem from this perspective.
constraint_indexing:constraint_classification(municipal_infrastructure_funding, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 4: INFRASTRUCTURE CONSULTING INDUSTRY (ROPE) — Organized actors (engineering firms, design consultants, project managers) capture value through grant administration complexity. Structured as coordination but with captured benefits. High arbitrage — can shift to private projects or consulting markets if municipal work dries up. Experiences the constraint as coordination with embedded profit capture.
constraint_indexing:constraint_classification(municipal_infrastructure_funding, rope,
    context(agent_power(organized),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 5: MUNICIPAL BOND MARKET (TANGLED ROPE) — Powerful actors (investment banks, pension funds, credit rating agencies) have high mobility (can move capital elsewhere) but maintain coordination function: they enable municipalities to borrow for infrastructure. The constraint is genuinely hybrid: without the bond market, municipalities cannot fund major projects. But the market extracts through interest premiums, covenant requirements, and rating discipline. Both coordination (enables borrowing) and extraction (captures interest spread, requires privatization concessions) are present. Mobile exit option means the market experiences moderate rather than maximal extraction.
constraint_indexing:constraint_classification(municipal_infrastructure_funding, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(regional))).

% PERSPECTIVE 6: ESSENTIAL SERVICE PROVISION SYSTEM (SNARE) — Cannot organize, has no exit option, bears full cost of underfunding. Water systems, sewers, roads are abstract collective goods. When underfunded, they degrade silently until catastrophic failure (Flint water crisis, bridge collapse, sanitary sewer overflow). The constraint extracts by deferring maintenance costs onto future generations. Experienced extraction is maximal from the perspective of long-term system reliability.
constraint_indexing:constraint_classification(municipal_infrastructure_funding, snare,
    context(agent_power(powerless),
            time_horizon(generational),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 7: MUNICIPAL FINANCE REGULATORY FRAMEWORK (PITON) — Legally mandated balanced budget requirements, debt service coverage ratios, and grant application bureaucracy are largely performative from a functional perspective: they constrain municipal flexibility without preventing the underlying extraction (deferred maintenance). The regulatory framework persists through institutional inertia, but its primary function — preventing fiscal catastrophe — has degraded. Municipalities work around the framework (special districts, off-balance-sheet vehicles) rather than complying with its spirit. Theater ratio high; effective function low.
constraint_indexing:constraint_classification(municipal_infrastructure_funding, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 8: INFRASTRUCTURE RENEWAL COALITION (SCAFFOLD) — Organized actors (mayors associations, labor unions, infrastructure advocacy groups) are building alternative pathways: dedicated infrastructure banks, revenue-backed bonds, federal direct lending. These create exits from the traditional (constrained) municipal finance model. The coalition sees the current constraint as temporary — a coordination failure that alternative funding structures can solve. Sunset clause: if federal infrastructure funding expands or state infrastructure banks mature, reliance on debt markets declines.
constraint_indexing:constraint_classification(municipal_infrastructure_funding, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / NATURAL LAW VIEW (MOUNTAIN) — From a civilizational perspective, the constraint may appear as an immutable property of urban governance: large infrastructure requires capital aggregation, capital aggregation requires credit systems, credit systems require discipline mechanisms. From this view, the extraction is 'the price of coordination.' However, the structural data contradicts the mountain classification — the constraint is a contingent institutional arrangement (specific funding rules, specific debt structures, specific regulatory frameworks), not a natural law.
constraint_indexing:constraint_classification(municipal_infrastructure_funding, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(municipal_infrastructure_funding_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(municipal_infrastructure_funding, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(municipal_infrastructure_funding, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(municipal_infrastructure_funding, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(municipal_infrastructure_funding, TR),
    TR >= 0.70.

:- end_tests(municipal_infrastructure_funding_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.52): High-moderate. The constraint extracts through multiple mechanisms: (1) debt service concentrates on taxpayers while benefits accrue to bond holders; (2) deferred maintenance externalizes costs across generations; (3) regulatory covenant requirements force privatization concessions. The trajectory shows extractiveness increasing from 0.28 (1980s: federal infrastructure support high, debt manageable) to 0.52 (2020s: federal support reduced, debt service consuming 15-25% of municipal budgets). Suppression (0.65): High. Residents face significant barriers to exit: property ties, high relocation costs, inadequate housing alternatives. State and federal constraints (balanced budget rules, debt ceilings) limit municipal flexibility. Bond covenants constrain policy autonomy. Theater ratio (0.58): Moderate-high. Grant application bureaucracy and balanced budget requirements perform fiscal discipline but do not prevent underlying extraction. Municipal financial reports present balanced budgets while deferring maintenance. Regulatory frameworks constrain without enabling genuine alternatives. Theater has increased as the gap between formal compliance and actual capital adequacy has widened.
 *
 * PERSPECTIVAL GAP:
 *   The constraint demonstrates sharp perspectival divergence. The bond market and state/federal administrators classify it as rope (pure coordination with minimal extraction from their perspective). Low-income residents classify it as snare (pure extraction, no coordination benefit to them). The homeowner classifies it as tangled rope (genuine coordination of infrastructure maintenance, but asymmetric extraction through debt burden). The regulatory framework sees itself as piton (ritualistic enforcement persisting through inertia). The renewal coalition sees scaffold with sunset (alternative funding models are maturing). The analytical observer risks seeing mountain (naturalizing contingent arrangements as immutable fiscal laws). The gap reflects power asymmetries: those with high mobility and arbitrage options experience the constraint as enabling coordination; those with low mobility and constrained options experience pure extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint's directionality derives from the flow of extraction: bond market holders benefit (low d), residents bear costs (high d), state/federal administrators reallocate resources (low d). The beneficiary declarations (senior debt holders, grant administrators) and victim declarations (low-income residents, essential service provision) create asymmetric power flows. A middle-income homeowner occupies an intermediate position — they benefit from infrastructure coordination but bear debt service costs, justifying the tangled rope classification (mixed benefits and extraction). The regulatory framework's ambiguous directionality (constraints limit local autonomy but serve state/federal interests) explains why it appears as piton: institutionally maintained despite degraded function.
 *
 * MANDATROPHY ANALYSIS:
 *   Mandatrophy is resolved by recognizing that the constraint is genuinely hybrid (tangled rope) at the aggregate level, but appears as different types from different perspectives because the extraction is not uniformly experienced. The beneficiaries (bond market, grant administrators) see rope because they capture net benefits. The victims (low-income residents, infrastructure systems) see snare because extraction is one-directional. The constraint cannot be mislabeled as 'pure coordination' (rope) from a powerless agent's perspective — they are trapped and paying costs. It cannot be mislabeled as 'pure extraction' (snare) from the bond market's perspective — genuine coordination function exists (municipalities need capital, markets provide it). The tangled rope classification at the aggregate level resolves the mandatrophy by acknowledging both the coordination function (infrastructure is a collective good) and the extraction mechanism (benefits are captured asymmetrically). The challenge is preventing policy framings that naturalize the extraction as inherent to 'how municipal finance works' — this is the false mountain risk (Perspective 9) that the analytical observer confronts.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    deferred_maintenance_threshold,
    'At what level of deferred maintenance does the system shift from sustainable deficit to catastrophic failure mode?',
    'Engineering analysis of infrastructure condition indices (ASCE report cards); correlation between maintenance spending levels and failure rates by system type (water, sewer, roads, bridges)',
    'If threshold is low: current underfunding is already driving catastrophic failure (Snare classification confirmed from essential services perspective). If threshold is high: some underfunding is tolerable (Tangled Rope is stable). Determines whether the constraint is extracting in real-time or deferring extraction across generations.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(deferred_maintenance_threshold, empirical, 'Deferred maintenance threshold to catastrophic failure').

omega_variable(
    grant_displacement_of_local_tax_revenue,
    'Do state and federal grants displace local tax revenue (state cuts its own support, federal grant fills gap) or do they supplement genuine new investment?',
    'Time-series analysis of local tax revenue vs state/federal grants by municipality; identification of gap-filling vs additive patterns; budget documentation from pre- and post-grant eras',
    'If displaces: grants function as extraction mechanism (federal government transfers fiscal discipline to municipalities, centralizes control). Rope classification (from state/federal perspective) is actually false — it''s not coordination, it''s mandate transfer. If supplements: grants genuinely solve coordination problems (legitimize Rope classification). Determines whether the constraint benefits local residents or primarily reallocates power from local to state/federal level.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(grant_displacement_of_local_tax_revenue, empirical, 'Whether grants supplement or displace local tax revenue').

omega_variable(
    privatization_extraction_quantification,
    'What proportion of municipal infrastructure value is extracted through privatization deals (public-private partnerships, managed competition, asset sales)?',
    'Systematic accounting of PPP concessions: revenue capture by private operators, service quality changes post-privatization, cost comparisons (public operation vs private contract); case study analysis of specific privatizations (parking, water, toll roads)',
    'If high: privatization is primary extraction mechanism alongside debt service (Snare classification from resident perspective is intensified). If low: privatization is minority phenomenon (Tangled Rope with modest extraction is stable). Determines whether residents experience the constraint as funding insufficiency or ownership transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(privatization_extraction_quantification, empirical, 'Proportion of infrastructure value extracted through privatization').

omega_variable(
    bond_market_dependency_exit_cost,
    'What structural changes would be required for municipalities to exit the municipal bond market entirely (shift to direct public lending, infrastructure banks, tax-backed revenue)?',
    'Policy analysis of alternative funding models (infrastructure banks in other countries, direct public lending proposals); fiscal modeling of transition costs and benefits; identification of structural dependencies (accounting standards, revenue pledging, covenant requirements)',
    'If exit cost is prohibitive: bond market has maximum structural power (Snare from municipal perspective). If exit cost is moderate: bond market is mobile actor with legitimate arbitrage options (Rope or Tangled Rope is stable). Determines whether the bond market''s position is structural or contestable.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(bond_market_dependency_exit_cost, empirical, 'Cost structure for exiting municipal bond market dependency').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(municipal_infrastructure_funding, 0, 20).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(mif_tr_t0, municipal_infrastructure_funding, theater_ratio, 0, 0.35).
narrative_ontology:measurement(mif_tr_t10, municipal_infrastructure_funding, theater_ratio, 10, 0.48).
narrative_ontology:measurement(mif_tr_t20, municipal_infrastructure_funding, theater_ratio, 20, 0.58).

% Extraction over time
narrative_ontology:measurement(mif_be_t0, municipal_infrastructure_funding, base_extractiveness, 0, 0.28).
narrative_ontology:measurement(mif_be_t10, municipal_infrastructure_funding, base_extractiveness, 10, 0.42).
narrative_ontology:measurement(mif_be_t20, municipal_infrastructure_funding, base_extractiveness, 20, 0.52).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(municipal_infrastructure_funding, resource_allocation).
narrative_ontology:boltzmann_floor_override(municipal_infrastructure_funding, 0.18).
narrative_ontology:affects_constraint(municipal_infrastructure_funding, public_pension_underfunding).
narrative_ontology:affects_constraint(municipal_infrastructure_funding, water_system_privatization).
narrative_ontology:affects_constraint(municipal_infrastructure_funding, transit_system_dependency).

% DUAL FORMULATION NOTE:
% Municipal infrastructure funding decomposes into multiple structurally distinct constraints with different ε values: (1) debt service burden (ε≈0.55, Snare from resident perspective); (2) maintenance deferral cycle (ε≈0.62, Tangled Rope with temporal asymmetry); (3) grant allocation politics (ε≈0.35, Rope with modest extraction). This story addresses the aggregate constraint. Network links to pension underfunding (correlated budget competition), water privatization (specific extraction mechanism), and transit dependency (alternative coordination failure).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(municipal_infrastructure_funding, institutional, 0.42).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
