% ============================================================================
% CONSTRAINT STORY: negative_emissions_arbitrage
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_negative_emissions_arbitrage, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: negative_emissions_arbitrage
 * human_readable: Negative Emissions Arbitrage Market
 * domain: economic/political/technological
 * * SUMMARY:
 * The scientific consensus that global warming stops only at "net-zero" emissions created a global carbon ledger. This constraint is the market mechanism built upon that ledger, where the high cost of absolute industrial decarbonization is arbitraged against the cost of "equivalent" atmospheric carbon removal technologies. It functions as a coordination mechanism for global capital but extracts value from legacy industries.
 * * KEY AGENTS:
 * - High-Intensity Legacy Emitters: Subject (Powerless) - Industries unable to achieve absolute-zero emissions, forced to buy removals.
 * - Removal Technology Providers: Beneficiary (Institutional) - Carbon capture firms, tech providers, and nations with high carbon-sink capacity who sell removal credits.
 * - Climate Policy Auditor: Auditor (Analytical) - Observers analyzing the market's structure, effectiveness, and extractive properties.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(negative_emissions_arbitrage, 0.82). % Snare extraction >= 0.46
domain_priors:suppression_score(negative_emissions_arbitrage, 0.55).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(negative_emissions_arbitrage, 0.15).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(negative_emissions_arbitrage, extractiveness, 0.82).
narrative_ontology:constraint_metric(negative_emissions_arbitrage, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(negative_emissions_arbitrage, theater_ratio, 0.15).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(negative_emissions_arbitrage, tangled_rope).

% Binary flags
domain_priors:requires_active_enforcement(negative_emissions_arbitrage). % Required for Tangled Rope

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(negative_emissions_arbitrage, removal_technology_providers).
narrative_ontology:constraint_victim(negative_emissions_arbitrage, high_intensity_legacy_emitters).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE LEGACY EMITTER (SNARE)
% For firms that cannot decarbonize to absolute-zero, the mandatory purchase
% of "equivalent removals" is a Snare, extracting profit to fund another sector.
constraint_indexing:constraint_classification(negative_emissions_arbitrage, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE REMOVAL PROVIDER (ROPE)
% Views the net-zero ledger as a pure Rope—a functional market mechanism to
% coordinate the balancing of emissions and removals, creating a new industry.
constraint_indexing:constraint_classification(negative_emissions_arbitrage, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The system is a Tangled Rope. It has a genuine coordination function (balancing
% the global carbon ledger) but also features high, asymmetric extraction from
% one group (emitters) to another (removers), all maintained by active policy
% enforcement.
constraint_indexing:constraint_classification(negative_emissions_arbitrage, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(negative_emissions_arbitrage_tests).

test(perspectival_gap) :-
    % Verify the gap between the emitter (Snare), provider (Rope), and analyst (Tangled Rope).
    constraint_indexing:constraint_classification(negative_emissions_arbitrage, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(negative_emissions_arbitrage, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(negative_emissions_arbitrage, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_properties) :-
    % A Tangled Rope requires beneficiaries, victims, and active enforcement.
    narrative_ontology:constraint_beneficiary(negative_emissions_arbitrage, _),
    narrative_ontology:constraint_victim(negative_emissions_arbitrage, _),
    domain_priors:requires_active_enforcement(negative_emissions_arbitrage).

:- end_tests(negative_emissions_arbitrage_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The high extraction score (0.82) reflects the mandatory capital flow from legacy emitters to the new carbon removal sector. The suppression score (0.55) reflects how the "net-zero" consensus has displaced alternative frameworks (like a fixed emissions budget).
 * The key insight is the distinction between the underlying physical law and the socio-economic system built upon it. The physical necessity of net-zero to stop warming is a Mountain. However, the *market for arbitrage* created to achieve this is a classic Tangled Rope: it solves a coordination problem (balancing the carbon ledger) while simultaneously enabling massive, asymmetric value extraction, all held in place by active regulation.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * This constraint is a textbook case of mandatrophy resolution via the Tangled Rope classification. A naive analysis might see only the Snare experienced by emitters or the Rope experienced by providers. The Tangled Rope classification correctly identifies that the system possesses *both* a genuine coordination function and a coercive, extractive one. This prevents the system from misclassifying a complex regulatory market as either pure coordination or pure predation.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_negative_emissions_arbitrage_1,
    'Are industrial removals truly equivalent in atmospheric effect and permanence to avoided fossil emissions?',
    'Long-term verification of carbon residence times in geological storage vs. atmospheric cycles.',
    'If equivalent: Tangled Rope holds. If not: The system degrades into a pure Snare based on fraudulent accounting.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(negative_emissions_arbitrage, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensified over time as the market matured and became more
% formalized, increasing both its extractive efficiency and performative aspects.
%
% Theater ratio over time (metric_substitution):
narrative_ontology:measurement(nea_tr_t0, negative_emissions_arbitrage, theater_ratio, 0, 0.05).
narrative_ontology:measurement(nea_tr_t5, negative_emissions_arbitrage, theater_ratio, 5, 0.10).
narrative_ontology:measurement(nea_tr_t10, negative_emissions_arbitrage, theater_ratio, 10, 0.15).

% Extraction over time (extraction_accumulation):
narrative_ontology:measurement(nea_ex_t0, negative_emissions_arbitrage, base_extractiveness, 0, 0.60).
narrative_ontology:measurement(nea_ex_t5, negative_emissions_arbitrage, base_extractiveness, 5, 0.75).
narrative_ontology:measurement(nea_ex_t10, negative_emissions_arbitrage, base_extractiveness, 10, 0.82).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: A carbon market is a form of resource allocation,
% managing the "resource" of atmospheric absorption capacity.
narrative_ontology:coordination_type(negative_emissions_arbitrage, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */