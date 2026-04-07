% ============================================================================
% CONSTRAINT STORY: coal_phase_out_timelines
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2026-02-26
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_coal_phase_out_timelines, []).

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
 *   constraint_id: coal_phase_out_timelines
 *   human_readable: Coal Phase-Out Timeline Commitments and Implementation
 *   domain: energy_policy/climate/geopolitics
 *
 * SUMMARY:
 *   Coal phase-out timelines represent a global climate policy constraint
 *   that creates asymmetric extraction from coal-dependent communities and
 *   nations while benefiting renewable energy investors and climate-committed
 *   developed economies. The constraint combines genuine coordination
 *   function (collective commitment to decarbonize enables renewable
 *   investment certainty) with substantial extraction mechanism (costs borne
 *   by powerless coal workers and developing nations with limited
 *   alternatives). The extractiveness has increased over time as enforcement
 *   mechanisms (carbon pricing, grid connection restrictions, capital market
 *   exclusion) have become more binding, while theater has remained high as
 *   international climate governance processes maintain ritual activity
 *   disconnected from implementation. This constraint demonstrates the full
 *   typology: snare from the coal miner's perspective (trapped,
 *   identity_locked), tangled rope from organized labor's perspective
 *   (coordination of just transition + extraction of concessions), rope from
 *   renewable investors (pure coordination benefit), scaffold from developed
 *   economies (temporary mechanism with sunset as renewable dominance
 *   approaches), piton from international climate institutions (high theater,
 *   degraded enforcement), and arguably a false mountain from the
 *   analytical/physical carbon limit perspective (which naturalizes the
 *   contingent political failure to enforce phase-outs).
 *
 * KEY AGENTS:
 *   - Coal mining workers and communities: Primary victims (powerless/trapped, identity_locked) — bear extraction through job loss, wage suppression, community dissolution, and inadequate transition support
 *   - Coal-dependent developing nations: Primary victims (powerless/identity_locked) — structurally mobile but identity-locked into coal-development pathway; constrained by capital requirements and technology access
 *   - Coal industry companies: Powerful victims (powerful/constrained) — face asset stranding and market access restriction; extract concessions through political negotiation
 *   - Labor unions and worker organizations: Secondary victims (organized/constrained) — coordinate just transition provisions but extract concessions on wages and workforce reduction
 *   - Renewable energy investors: Primary beneficiaries (institutional/arbitrage) — benefit from investment certainty and subsidy frameworks; experience constraint as coordination
 *   - Developed climate-committed economies: Powerful beneficiaries (powerful/mobile) — set phase-out timelines; experience as temporary scaffold with visible exit path
 *   - International climate institutions: Institutional observers (institutional/constrained) — maintain governance framework with high theater but degraded enforcement (piton)
 *   - Analytical observer: Civilizational view (analytical/analytical) — risks naturalizing coal's physical carbon content as justification for failing to enforce political phase-out
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(coal_phase_out_timelines, 0.58).
domain_priors:suppression_score(coal_phase_out_timelines, 0.65).
domain_priors:theater_ratio(coal_phase_out_timelines, 0.68).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(coal_phase_out_timelines, extractiveness, 0.58).
narrative_ontology:constraint_metric(coal_phase_out_timelines, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(coal_phase_out_timelines, theater_ratio, 0.68).

% --- Constraint claim ---
narrative_ontology:constraint_claim(coal_phase_out_timelines, tangled_rope).
narrative_ontology:human_readable(coal_phase_out_timelines, "Coal Phase-Out Timeline Commitments and Implementation").
narrative_ontology:topic_domain(coal_phase_out_timelines, "energy_policy/climate/geopolitics").

domain_priors:requires_active_enforcement(coal_phase_out_timelines).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(coal_phase_out_timelines, renewable_energy_investors).
narrative_ontology:constraint_beneficiary(coal_phase_out_timelines, climate_advocacy_organizations).
narrative_ontology:constraint_beneficiary(coal_phase_out_timelines, developed_nations_low_carbon_transition).
narrative_ontology:constraint_victim(coal_phase_out_timelines, coal_mining_communities).
narrative_ontology:constraint_victim(coal_phase_out_timelines, coal_dependent_economies).
narrative_ontology:constraint_victim(coal_phase_out_timelines, developing_nations_energy_access).
narrative_ontology:constraint_victim(coal_phase_out_timelines, coal_workers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: COAL MINING COMMUNITY (SNARE) — Locked into coal-dependent economy by geographic immobility, skill specificity, and lack of alternative employment. Face extraction through wage suppression during transition (company threats to close operations), regulatory uncertainty, and inadequate reskilling programs. No exit option exists within biographical timeframe; suppression is structural (economic dependency) and internalized (identity as coal workers).
constraint_indexing:constraint_classification(coal_phase_out_timelines, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: COAL-DEPENDENT DEVELOPING NATIONS (SNARE) — Structurally mobile (could shift to renewables) but identity-locked into coal-dependent industrialization pathway. National identity and development legitimacy constructed through coal energy expansion. Exit requires abandoning the development model that constitutes state identity and sovereignty claim. Suppression combines structural barriers (capital requirements, technology access) with internalized framing (coal as marker of industrial modernity).
constraint_indexing:constraint_classification(coal_phase_out_timelines, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(identity_locked),
            spatial_scope(national))).

% PERSPECTIVE 3: LABOR UNIONS AND WORKER ORGANIZATIONS (TANGLED ROPE) — Organized agents with genuine coordination function (negotiating just transition provisions, training programs, pension protection) alongside asymmetric extraction (concessions on wage levels, workforce reduction, pension cuts in exchange for transition deal). Constrained by political economy — cannot prevent closure but can shape terms of transition.
constraint_indexing:constraint_classification(coal_phase_out_timelines, tangled_rope,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: RENEWABLE ENERGY COMPANIES (ROPE) — Institutional beneficiaries with arbitrage options (can move capital to any jurisdiction with favorable policy). Experience constraint as pure coordination: phase-out timelines create investment certainty, signal stable demand for renewables, and eliminate fossil fuel subsidy competition. Net benefit with minimal experienced extraction.
constraint_indexing:constraint_classification(coal_phase_out_timelines, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 5: COAL INDUSTRY (TANGLED ROPE) — Powerful actors facing constraint enforcement (regulatory phase-outs, carbon pricing, stranded asset laws). Experience extraction through asset devaluation, restricted access to capital markets, and regulatory acceleration. But also coordinate with governments on managed decline timelines, early-exit compensation, and transition financing. Constrained (cannot prevent phase-out but can negotiate terms and delay).
constraint_indexing:constraint_classification(coal_phase_out_timelines, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(continental))).

% PERSPECTIVE 6: DEVELOPED ECONOMIES (SCAFFOLD) — Powerful actors with mobile options who can set phase-out timelines with sunset clause: 2030-2040 for coal generation in EU/UK, 2035-2050 in US. See constraint as temporary coordination mechanism to decarbonize economically while managing political resistance. Theater high (net-zero commitments) but declining as implementation matures. Organized (EU Emissions Trading System, Paris Agreement) with exit path visible.
constraint_indexing:constraint_classification(coal_phase_out_timelines, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 7: INTERNATIONAL CLIMATE INSTITUTIONS (PITON) — COP processes, UN climate frameworks, IPCC maintain extensive ritualistic activity (commitments, reviews, reporting) with limited enforcement power. Theater ratio high (negotiation, symbolic declarations) but functional enforcement degraded (no binding mechanism, default on loss-and-damage funds, gap between stated and implemented targets). Persists through institutional inertia and soft legitimacy rather than causal effectiveness.
constraint_indexing:constraint_classification(coal_phase_out_timelines, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 8: OPERATING COAL PLANTS (PITON) — Individual coal power plants as institutional actors increasingly operate as zombies: uneconomic relative to renewables, operating below nameplate capacity, maintained through subsidy and grid support requirements (ramping capacity, frequency regulation) that justify their continued operation despite economic obsolescence. Theater high (grid stabilization rationale); actual function degraded (renewables + storage increasingly provide those services). Maintained through institutional inertia.
constraint_indexing:constraint_classification(coal_phase_out_timelines, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 9: ANALYTICAL OBSERVER / PHYSICAL CARBON LIMIT (MOUNTAIN) — From physical/thermodynamic perspective, the carbon budget for 1.5°C warming is a hard constraint. All coal that gets burned above the budget ceiling represents violation of physical law. No negotiation, no exit, no rescheduling changes the caloric content or carbon output of coal. However, this mountain classification naturalizes contingent political/economic failure to constrain coal use — the phase-out timeline constraint is not the physical limit but the institutional struggle to honor it.
constraint_indexing:constraint_classification(coal_phase_out_timelines, mountain,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(coal_phase_out_timelines_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(coal_phase_out_timelines, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(coal_phase_out_timelines, TypeOther, context(agent_power(organized), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(coal_phase_out_timelines, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(coal_phase_out_timelines, TR),
    TR >= 0.70.

:- end_tests(coal_phase_out_timelines_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.58): Moderate-high, increasing over time from 0.32 to 0.62. The constraint has become more extractive as enforcement mechanisms have tightened (carbon pricing, grid connection restrictions, capital market exclusion) and renewable dominance has eliminated alternative pathways for coal. Early in the interval, coal remained economically viable in some markets; later, phase-out becomes structural rather than policy choice, increasing experienced extraction for those locked into coal systems. Suppression (0.65): High. Barriers to exit for coal communities include occupational skill specificity, geographic immobility, lack of alternative regional employment, capital barriers to transition (retraining costs), and structural unemployment risk. For developing nations: capital constraints, technology access barriers, and energy access requirements. Theater ratio (0.68): Moderate-high. International climate commitments (Paris Agreement, net-zero pledges) create high theater — extensive negotiation, reporting, symbolic declarations — while actual coal retirement rates remain below stated timelines in many jurisdictions. Operating coal plants are maintained through grid support justifications (frequency regulation, ramping) increasingly handled by renewables + storage. Claimed type (Tangled Rope): Satisfies gates — beneficiaries (renewable investors, developed nations), victims (coal workers/communities), active enforcement (carbon pricing, regulatory restrictions). But manifests differently from different perspectives because directionality varies sharply: beneficiaries see rope, victims see snare, organized agents see mixed coordination-extraction.
 *
 * PERSPECTIVAL GAP:
 *   The primary gap is between beneficiaries with high exit (rope perspective) and victims with low exit (snare perspective). Renewable investors see constraint as coordination solution; coal miners see it as structural extraction. The secondary gap is between developed economies setting timelines (scaffold: temporary, managed, exit visible) and developing economies subject to timelines (snare: identity-locked, no exit path). The tertiary gap is between actual coal retirement trajectories and stated phase-out commitments — the piton perspective reveals high theater (extensive COP activity, net-zero pledges) disconnected from implementation mechanisms. These gaps are not perspectival ambiguity but structural evidence of asymmetric distribution.
 *
 * DIRECTIONALITY LOGIC:
 *   Each agent's experienced extractiveness (χ) is computed from base extractiveness (ε ≈ 0.58), directionality f(d), and scope σ(S). Coal workers (d=0.95, local scope σ=0.8): χ ≈ 0.58 × 1.42 × 0.8 ≈ 0.66 (snare). Renewable investors (d=0.05, global scope σ=1.2): χ ≈ 0.58 × (-0.12) × 1.2 ≈ -0.08 (rope, negative extraction, subsidy flowing toward them). Labor unions (d=0.55, national scope σ=1.0): χ ≈ 0.58 × 0.75 × 1.0 ≈ 0.44 (tangled rope). This derivation chain explains why the same base ε produces different classifications from different positions — the multiplier (f(d) × σ(S)) changes the experienced extractiveness dramatically.
 *
 * MANDATROPHY ANALYSIS:
 *   The coal phase-out constraint resolves the mandatrophy by demonstrating that classification type is indexical. The constraint is simultaneously snare (coal miners), tangled rope (labor, coal industry), rope (renewable investors), scaffold (developed economies), and piton (climate institutions). These are not competing claims about 'what it really is' but accurate descriptions of different agents' structural relationships to the same constraint. The mandatrophy resolution shows that the constraint's 'true type' only exists relative to a specific agent position. The analytical observer's mountain classification (physical carbon limit) is revealed as a false summit — it naturalizes the contingent political failure to enforce phase-outs. The physical constraint is the carbon budget; the phase-out timeline is the institutional struggle to honor it. Mandatrophy is resolved by refusing to collapse the perspectival range into a single type and instead reading the range itself as diagnostic: the spread across all six types indicates that costs and benefits are radically unequally distributed, that the constraint's enforcement relies on suppressing alternatives (high suppression = 0.65), and that theater ratio is increasing faster than functional enforcement (theater_ratio trending toward 0.71 while phase-out timelines slip). The constraint will be resolved when: (a) renewable energy dominates grid dynamics sufficiently that coal is economically unviable (not policy-enforced), eliminating the snare mechanism, or (b) just transition mechanisms fully compensate coal communities, converting snare to tangled rope and eventually rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    just_transition_scope,
    'What constitutes ''just transition'' — sufficient retraining, pension security, and community investment to prevent large-scale precarity?',
    'Post-closure tracking of worker employment, wage trajectories, health outcomes, and community economic indicators in coal regions with vs without comprehensive transition funding',
    'If transition programs fully compensate: suppression may be overstated, classification shifts toward Scaffold. If transition programs are inadequate: snare classification confirmed, extraction persists post-closure through intergenerational poverty.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(just_transition_scope, empirical, 'Whether just transition provisions adequately compensate coal workers and communities').

omega_variable(
    renewable_resource_constraints,
    'Can renewable energy infrastructure scale fast enough to replace coal generation without requiring new extraction constraints (critical minerals, rare earth processing, land use)?',
    'Supply chain analysis of solar/wind/battery materials; comparison of extraction/labor conditions in renewable supply chains vs coal mining',
    'If renewables are truly lower-extraction: phase-out is net gain. If renewables displace extraction to different populations/geographies: constraint is redistributed, not eliminated.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(renewable_resource_constraints, empirical, 'Whether renewable energy scales without new extraction constraints').

omega_variable(
    timeline_credibility_gap,
    'Are national phase-out commitments credible, or do they primarily serve rhetorical/electoral purposes without implementation mechanism?',
    'Comparison of stated timelines vs coal plant retirements, electricity generation volumes, and capital investment trajectories. Analysis of 2010-2020 phase-out commitment slippage.',
    'If highly credible: constraint is binding, extraction mechanism real. If rhetorical: constraint is Piton (theater > function), victim populations cannot rely on timelines for transition planning.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(timeline_credibility_gap, empirical, 'Credibility of national coal phase-out commitments versus actual implementation').

omega_variable(
    developing_nation_energy_justice,
    'Does enforcement of global coal phase-out timelines constitute climate justice or energy colonialism — imposing low-carbon pathways on nations still below energy access thresholds?',
    'Analysis of per-capita electricity access, development indicators, and carbon budget allocation between developed and developing nations. Outcome data on electrification rates in transition zones.',
    'If enforcement prevents energy access: snare classification valid, high suppression justified. If alternative paths (renewable expansion, grid interconnection, efficiency) enable development: tangled rope or scaffold may better fit.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(developing_nation_energy_justice, preference, 'Whether phase-out enforcement enables or blocks energy access in developing nations').

omega_variable(
    stranded_asset_externality,
    'Who bears the cost of coal asset stranding — coal companies, shareholders, workers, or dispersed across populations through supply chain disruption?',
    'Financial analysis of write-downs, bailout mechanisms, and pension fund impacts. Tracing of supply chain disruption costs.',
    'If concentrated: extraction mechanism is clear transfer from investors to energy transition. If dispersed: suppression is higher because workers/communities bear costs they didn''t create.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(stranded_asset_externality, empirical, 'Who bears the cost of coal asset stranding').

omega_variable(
    identity_lock_mechanism_interpersonal,
    'For coal workers with identity_locked exit options, is the binding mechanism occupational identity (coal miner as career path), community identity (coal town culture), or political identity (coal as marker of working-class standing)?',
    'Qualitative research on worker narratives post-transition; analysis of whether workers exit into alternative employment at wage parity or exit the labor force entirely',
    'If occupational only: retraining may be sufficient to unlock exit. If community or political: identity frame must shift for exit to occur; timeline extends beyond job availability.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(identity_lock_mechanism_interpersonal, empirical, 'Nature of identity lock mechanism for coal workers').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(coal_phase_out_timelines, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(coal_tr_t0, coal_phase_out_timelines, theater_ratio, 0, 0.52).
narrative_ontology:measurement(coal_tr_t10, coal_phase_out_timelines, theater_ratio, 10, 0.65).
narrative_ontology:measurement(coal_tr_t20, coal_phase_out_timelines, theater_ratio, 20, 0.68).
narrative_ontology:measurement(coal_tr_t30, coal_phase_out_timelines, theater_ratio, 30, 0.71).

% Extraction over time
narrative_ontology:measurement(coal_be_t0, coal_phase_out_timelines, base_extractiveness, 0, 0.32).
narrative_ontology:measurement(coal_be_t10, coal_phase_out_timelines, base_extractiveness, 10, 0.48).
narrative_ontology:measurement(coal_be_t20, coal_phase_out_timelines, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(coal_be_t30, coal_phase_out_timelines, base_extractiveness, 30, 0.62).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(coal_phase_out_timelines, resource_allocation).
narrative_ontology:affects_constraint(coal_phase_out_timelines, renewable_energy_investment_certainty).
narrative_ontology:affects_constraint(coal_phase_out_timelines, carbon_pricing_mechanisms).
narrative_ontology:affects_constraint(coal_phase_out_timelines, energy_access_developing_nations).
narrative_ontology:affects_constraint(coal_phase_out_timelines, stranded_fossil_asset_externalities).

% DUAL FORMULATION NOTE:
% The coal phase-out constraint decomposes into at least three structurally distinct claims: (1) physical carbon budget (mountain, ε≈0.0), (2) renewable energy scaling (tangled rope, ε≈0.45), (3) just transition adequacy (snare if inadequate, ε≈0.70; rope if adequate, ε≈0.15). This story focuses on the policy enforcement constraint; upstream physical carbon limits and downstream transition mechanism stories would have distinct ε values and perspectives.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

constraint_indexing:directionality_override(coal_phase_out_timelines, institutional, 0.25).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
