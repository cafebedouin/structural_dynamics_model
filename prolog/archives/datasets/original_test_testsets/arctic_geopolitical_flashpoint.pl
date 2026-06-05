% ============================================================================
% CONSTRAINT STORY: arctic_geopolitical_flashpoint
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_arctic_geopolitical_flashpoint, []).

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
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: arctic_geopolitical_flashpoint
 *   human_readable: The Melting Ice and the Scramble for Greenland
 *   domain: geopolitical/economic
 *
 * SUMMARY:
 *   The rapid melting of Arctic ice is unlocking both new shipping lanes (the
 *   Northern Sea Route) and vast reserves of critical minerals under
 *   Greenland. This has transformed the region from a remote periphery into a
 *   central chessboard for geopolitical competition. The constraint is the
 *   emerging framework of treaties, military postures, and economic deals
 *   governing this scramble. It forces all actors—great powers, regional
 *   states, indigenous populations, and corporations—into a high-stakes game
 *   where the rules are being written in real-time.
 *
 * KEY AGENTS:
 *   - Great Powers (US, China, Russia): Primary beneficiaries (institutional/arbitrage) - seek strategic control of sea lanes and access to minerals.
 *   - Greenlandic Indigenous Population: Primary victims (powerless/trapped) - face environmental risk and potential loss of sovereignty for promised economic gains.
 *   - Denmark Government: Constrained sovereign (institutional/constrained) - attempts to manage its relationship with Greenland, NATO allies, and economic pressures.
 *   - Mining Corporations: Organized beneficiaries (organized/mobile) - seek to profit from resource extraction under temporary licenses.
 *   - International Environmental System: Degraded institution (institutional/constrained) - provides a forum for discussion but lacks enforcement power, becoming largely performative.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(arctic_geopolitical_flashpoint, 0.65).
domain_priors:suppression_score(arctic_geopolitical_flashpoint, 0.7).
domain_priors:theater_ratio(arctic_geopolitical_flashpoint, 0.75).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(arctic_geopolitical_flashpoint, extractiveness, 0.65).
narrative_ontology:constraint_metric(arctic_geopolitical_flashpoint, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(arctic_geopolitical_flashpoint, theater_ratio, 0.75).

% --- Constraint claim ---
narrative_ontology:constraint_claim(arctic_geopolitical_flashpoint, tangled_rope).
narrative_ontology:human_readable(arctic_geopolitical_flashpoint, "The Melting Ice and the Scramble for Greenland").
narrative_ontology:topic_domain(arctic_geopolitical_flashpoint, "geopolitical/economic").

domain_priors:requires_active_enforcement(arctic_geopolitical_flashpoint).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(arctic_geopolitical_flashpoint, great_powers).
narrative_ontology:constraint_beneficiary(arctic_geopolitical_flashpoint, mining_corporations).
narrative_ontology:constraint_victim(arctic_geopolitical_flashpoint, greenlandic_indigenous_population).
narrative_ontology:constraint_victim(arctic_geopolitical_flashpoint, denmark_government).
narrative_ontology:constraint_victim(arctic_geopolitical_flashpoint, global_climate_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: INDIGENOUS POPULATION (SNARE) — Trapped by geography and power asymmetry. The promise of mineral wealth acts as a lure, but the structural reality is the extraction of resources with severe environmental and cultural costs, and a loss of sovereignty to external powers. d≈0.95, f(d)≈1.42, σ=0.9 → χ≈0.83.
constraint_indexing:constraint_classification(arctic_geopolitical_flashpoint, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(regional))).

% PERSPECTIVE 2: GREAT POWERS (ROPE) — Experience the situation as a pure coordination game ('The Great Game'). They have the capital and military power to set the rules and can shift investment elsewhere (arbitrage). The constraint is about managing competition and access among peers. d≈0.05, f(d)≈-0.12, σ=1.2 → χ≈-0.09. Negative extraction indicates a net subsidy.
constraint_indexing:constraint_classification(arctic_geopolitical_flashpoint, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: DANISH GOVERNMENT (TANGLED ROPE) — As the nominal sovereign, Denmark is caught between managing Greenland's autonomy, pressure from great power allies/rivals, and its own economic interests. It cannot easily exit its treaty obligations or historical ties. It sees both the coordination function and the severe extractive pressure. d≈0.75, f(d)≈1.10, σ=1.0 → χ≈0.72.
constraint_indexing:constraint_classification(arctic_geopolitical_flashpoint, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

% PERSPECTIVE 4: MINING CORPORATIONS (SCAFFOLD) — View their investment as a temporary project with a clear lifecycle (sunset clause: resource depletion). They are building the infrastructure for extraction, which requires coordination with states, but their commitment is finite. They can exit if the project becomes unprofitable. d≈0.55, f(d)≈0.75, σ=1.2 → χ≈0.58. This χ is high for a scaffold, but the sunset logic is dominant for this agent.
constraint_indexing:constraint_classification(arctic_geopolitical_flashpoint, scaffold,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 5: ENVIRONMENTAL TREATY SYSTEM (PITON) — Institutions like the Paris Agreement or the Arctic Council lack enforcement power here. Their function is largely performative—hosting summits and issuing statements while the resource scramble accelerates. The high theater_ratio (0.75) reflects this gap between function and performance, qualifying it as a Piton.
constraint_indexing:constraint_classification(arctic_geopolitical_flashpoint, piton,
    context(agent_power(institutional),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 6: ANALYTICAL OBSERVER (TANGLED ROPE) — The system has a genuine coordination function (managing new sea routes, deconflicting military activity) but is dominated by asymmetric extraction (mineral wealth flowing to external powers) and requires active enforcement (military presence, diplomatic coercion). This is the canonical Tangled Rope structure. d≈0.72, f(d)≈1.15, σ=1.2 → χ≈0.90.
constraint_indexing:constraint_classification(arctic_geopolitical_flashpoint, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(arctic_geopolitical_flashpoint_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(arctic_geopolitical_flashpoint, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(arctic_geopolitical_flashpoint, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(arctic_geopolitical_flashpoint, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(arctic_geopolitical_flashpoint, TR),
    TR >= 0.70.

:- end_tests(arctic_geopolitical_flashpoint_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.65) is high, representing the immense value of mineral wealth being claimed by external actors. Suppression (0.70) is high due to the military and economic power asymmetries that limit the choices of smaller actors like Greenland and Denmark. Theater Ratio (0.75) is high because the gap between the rhetoric of international cooperation, environmental protection, and indigenous rights, and the reality of a naked resource grab is vast. The constraint requires active enforcement through military presence and coercive diplomacy.
 *
 * PERSPECTIVAL GAP:
 *   The gap is extreme. For Great Powers, it's a coordination 'game' (Rope). For the indigenous population, it's a coercive trap (Snare). For Denmark, it's a complex hybrid of opportunity and threat (Tangled Rope). For environmental bodies, it's a failed, inertial process (Piton). This diversity of classification from a single set of base properties is a hallmark of a complex geopolitical constraint.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (Great Powers, Corporations) have arbitrage or mobile exit options, leading to low or negative effective extraction (χ). They see the system as a tool for coordination. Victims (Greenlandic Population, Denmark) are trapped or constrained, leading to high derived directionality (d) and thus high effective extraction (χ). They experience the system as coercive and extractive. The analytical view synthesizes these functions, identifying the structure as a Tangled Rope.
 *
 * MANDATROPHY ANALYSIS:
 *   This case resolves the mandatrophy by demonstrating that 'Rope' and 'Snare' are not mutually exclusive descriptions of reality, but valid perspectival slices of a more complex Tangled Rope structure. An analysis that only saw the Great Power coordination game would miss the extraction. An analysis that only saw the indigenous victimhood would miss the genuine coordination functions. The Deferential Realism framework correctly identifies the complete structure by integrating these perspectives.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    greenlandic_independence_viability,
    'Will the resource boom enable genuine Greenlandic independence or transform it into a dependent client state of the highest bidder?',
    'Tracking capital flows, terms of mining contracts, and political concessions made by the Greenlandic government over the next decade.',
    'If genuine independence: the ''victim'' agent gains power, potentially reclassifying the constraint. If client state: confirms the Snare classification for the local population.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(greenlandic_independence_viability, empirical, 'Whether resource wealth leads to true independence or client state status for Greenland.').

omega_variable(
    great_power_conflict_threshold,
    'Will the scramble be managed via a stable, albeit tense, equilibrium, or will it cross a threshold into direct military conflict?',
    'Monitoring military deployments, near-miss incidents, and escalatory rhetoric between NATO/US, Russia, and China in the GIUK gap and Northern Sea Route.',
    'Stable equilibrium confirms the Rope/Tangled Rope views. Direct conflict would represent a catastrophic failure of the coordination function, collapsing the constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(great_power_conflict_threshold, empirical, 'The likelihood of the Arctic scramble escalating into direct military conflict.').

omega_variable(
    nsr_economic_viability,
    'Is the Northern Sea Route a transformative global shipping lane, or is its economic importance overstated compared to the risks and seasonal limitations?',
    'Analysis of shipping volumes, insurance costs, and infrastructure investment vs. projections over the 2025-2030 period.',
    'If highly viable: the coordination function is real and significant, strengthening Rope/Tangled Rope classifications. If overstated: the ''coordination'' aspect is mostly theater, and the constraint is more purely about mineral extraction (Snare).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(nsr_economic_viability, empirical, 'The actual economic viability of the Northern Sea Route versus strategic hype.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(arctic_geopolitical_flashpoint, 2024, 2035).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(arct_tr_t2024, arctic_geopolitical_flashpoint, theater_ratio, 2024, 0.6).
narrative_ontology:measurement(arct_tr_t2030, arctic_geopolitical_flashpoint, theater_ratio, 2030, 0.7).
narrative_ontology:measurement(arct_tr_t2035, arctic_geopolitical_flashpoint, theater_ratio, 2035, 0.75).

% Extraction over time
narrative_ontology:measurement(arct_be_t2024, arctic_geopolitical_flashpoint, base_extractiveness, 2024, 0.5).
narrative_ontology:measurement(arct_be_t2030, arctic_geopolitical_flashpoint, base_extractiveness, 2030, 0.58).
narrative_ontology:measurement(arct_be_t2035, arctic_geopolitical_flashpoint, base_extractiveness, 2035, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(arctic_geopolitical_flashpoint, resource_allocation).
narrative_ontology:affects_constraint(arctic_geopolitical_flashpoint, critical_mineral_supply_chains).
narrative_ontology:affects_constraint(arctic_geopolitical_flashpoint, global_shipping_lanes).
narrative_ontology:affects_constraint(arctic_geopolitical_flashpoint, nato_cohesion).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
