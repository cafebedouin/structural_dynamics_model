% ============================================================================
% CONSTRAINT STORY: cuba_mandatrophic_collapse
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-21
% ============================================================================

:- module(constraint_cuba_mandatrophic_collapse, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: cuba_mandatrophic_collapse
 * human_readable: Cuban Mandatrophy (The GAESA-Infrastructure Divergence)
 * domain: political/economic/technological
 * * SUMMARY:
 * Mandatrophy in Cuba describes the terminal wasting away of national infrastructure and food security caused by the rigid prioritization of the "Military-Tourism Mandate" (GAESA) over the essential "margins" of the civilian state (the energy grid, agriculture, and public health). While the national power grid suffered multiple total collapses in 2024 and 2025 due to zero maintenance, the military conglomerate GAESA continued to extract nearly 40% of state investment into luxury hotel construction.
 * * KEY AGENTS:
 * - GAESA Executive: Beneficiary (Institutional)
 * - Cuban Citizen: Subject (Powerless)
 * - External Analyst: Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale (Extraction): 0.95. Extreme extraction; GAESA controls 95% of foreign currency transactions and 70% of the economy, yet none of these billions are diverted to fix the failing national power grid.
domain_priors:base_extractiveness(cuba_mandatrophic_collapse, 0.95).
% Rationale (Suppression): 0.85. The "War Economy" declaration suppresses domestic private alternatives, while military presence enforces a "no-protest" constraint.
domain_priors:suppression_score(cuba_mandatrophic_collapse, 0.85).
% Rationale (Theater): 0.10. The extraction is real and functional, not performative. The hotels are built, the money is moved.
domain_priors:theater_ratio(cuba_mandatrophic_collapse, 0.10).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(cuba_mandatrophic_collapse, extractiveness, 0.95).
narrative_ontology:constraint_metric(cuba_mandatrophic_collapse, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(cuba_mandatrophic_collapse, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% The state claims this is a necessary structure for economic survival (enforcement/coordination).
narrative_ontology:constraint_claim(cuba_mandatrophic_collapse, snare).
narrative_ontology:human_readable(cuba_mandatrophic_collapse, "Cuban Mandatrophy (The GAESA-Infrastructure Divergence)").
narrative_ontology:topic_domain(cuba_mandatrophic_collapse, "political/economic/technological").

% Binary flags
% Requires heavy active enforcement (CIMEX dollar-stores, military oversight of finance, and police crackdowns).
domain_priors:requires_active_enforcement(cuba_mandatrophic_collapse).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(cuba_mandatrophic_collapse, gaesa_military_elite).
narrative_ontology:constraint_beneficiary(cuba_mandatrophic_collapse, hotel_construction_sector).
narrative_ontology:constraint_victim(cuba_mandatrophic_collapse, national_electrical_union_une).
narrative_ontology:constraint_victim(cuba_mandatrophic_collapse, cuban_agricultural_producers).
narrative_ontology:constraint_victim(cuba_mandatrophic_collapse, general_population).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE CUBAN CITIZEN (SNARE)
% For the citizen, Mandatrophy is a Snare. The regime's "Mandate" (preserving
% the military-run tourism empire) has extracted their electricity,
% medicine, and food rations. Every peso they earn is "choked" by inflation.
constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, snare,
    context(agent_power(powerless),
            time_horizon(immediate),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE GAESA EXECUTIVE (ROPE)
% GAESA views its conglomerate as a Rope—a functional coordination mechanism
% to capture hard currency in an era of sanctions. They see the "extraction"
% of domestic resources into hotel towers as a necessary strategic
% investment to eventually "save" the economy through tourism.
constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% The analytical observer sees a classic Snare. The high base extraction (0.95)
% and suppression (0.85) combined with active enforcement and the existence of
% viable, suppressed alternatives (e.g., agricultural reform, grid maintenance)
% confirm a system of pure extraction, despite its coordination claims.
constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(cuba_mandatrophic_collapse_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(cuba_mandatrophic_collapse, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(snare_threshold_validation) :-
    % Verify the constraint meets the numerical signature of a Snare.
    config:param(extractiveness_metric_name, ExtMetricName),
    config:param(suppression_metric_name, SuppMetricName),
    narrative_ontology:constraint_metric(cuba_mandatrophic_collapse, ExtMetricName, E),
    narrative_ontology:constraint_metric(cuba_mandatrophic_collapse, SuppMetricName, S),
    E >= 0.46,
    S >= 0.60.

:- end_tests(cuba_mandatrophic_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of this constraint is the extreme divergence between the institutional and powerless perspectives, making it a canonical example of a high-extraction system. The extraction score of 0.95 is justified by GAESA's near-total control over the foreign currency economy, which it uses for its own projects while essential national infrastructure like the power grid collapses. The suppression score of 0.85 reflects the state's "War Economy" declaration and active suppression of protest and private enterprise.
 *
 * PERSPECTIVAL GAP:
 * The GAESA executive sees a Rope: a necessary tool to coordinate the capture of dollars for national survival. The citizen experiences a Snare: a trap that extracts their livelihood, energy, and food to fund projects they will never benefit from. The analytical view confirms the citizen's experience; the metrics point unequivocally to a Snare. The grid engineer's perspective of a "Mountain" (physical collapse) is the *result* of the Snare's long-term operation.
 *
 * [RESOLVED MANDATROPHY]: The system is correctly identified as a Snare from the analytical perspective due to its extreme extraction and suppression metrics, preventing misclassification as a necessary evil or an unchangeable Mountain.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_cuba_mandatrophic_collapse,
    "Does GAESA actually hold $14-18B in liquid reserves, or is this an accounting Mountain used to maintain the illusion of solvency?",
    "Audit of offshore BFI (Banco Financiero Internacional) transactions and GAESA's internal 'dollar gravity' ledgers.",
    "If reserves are real: Mandatrophy is a conscious policy choice (Snare). If reserves are fictional: The entire state is an insolvent Piton approaching a Mountain of collapse.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(cuba_mandatrophic_collapse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint intensified significantly after the 2021 monetary reforms,
% leading to the grid collapses of 2024-2025. The data models this
% accelerating extraction. Theater ratio remains low as the extraction is functional.

% Theater ratio over time (metric_substitution detection):
narrative_ontology:measurement(cmc_tr_t0, cuba_mandatrophic_collapse, theater_ratio, 0, 0.05).
narrative_ontology:measurement(cmc_tr_t5, cuba_mandatrophic_collapse, theater_ratio, 5, 0.08).
narrative_ontology:measurement(cmc_tr_t10, cuba_mandatrophic_collapse, theater_ratio, 10, 0.10).

% Extraction over time (extraction_accumulation detection):
narrative_ontology:measurement(cmc_ex_t0, cuba_mandatrophic_collapse, base_extractiveness, 0, 0.75).
narrative_ontology:measurement(cmc_ex_t5, cuba_mandatrophic_collapse, base_extractiveness, 5, 0.85).
narrative_ontology:measurement(cmc_ex_t10, cuba_mandatrophic_collapse, base_extractiveness, 10, 0.95).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% GAESA's function is a form of centralized, coercive resource allocation.
narrative_ontology:coordination_type(cuba_mandatrophic_collapse, resource_allocation).

% This constraint is structurally dependent on the inflow of foreign currency
% from family remittances, which it then extracts.
narrative_ontology:affects_constraint(cuba_remittance_dependency, cuba_mandatrophic_collapse).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */