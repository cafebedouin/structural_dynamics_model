% ============================================================================
% CONSTRAINT STORY: scurvy_maritime_extraction
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_scurvy_maritime_extraction, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: scurvy_maritime_extraction
 * human_readable: The Scurvy/Empire Trade-off
 * domain: biological/political/economic
 * * SUMMARY:
 * During the Age of Sail (15th-18th centuries), scurvy (Vitamin C deficiency) was a primary biological constraint on long-distance sea voyages. While the cure (citrus) was periodically discovered, institutional inertia and cost-cutting measures transformed this solvable medical issue into a system of lethal extraction. The constraint is not the disease itself (a Mountain of biology), but the socio-economic system that accepted mass death among sailors as a cost of building global maritime empires.
 * * KEY AGENTS:
 * - Common Sailor: Subject (Powerless), trapped on a vessel with a deficient diet, facing near-certain illness and death.
 * - Maritime Empires (e.g., British Admiralty): Beneficiary (Institutional), profiting from global trade and colonization enabled by these voyages, treating sailor mortality as a predictable operational cost.
 * - Medical Analyst (e.g., James Lind): Observer (Analytical), who identifies the biological reality and the simple cure, but whose findings are ignored by the institution.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Rationale: Extraction is near-maximal, representing the direct extraction of life.
domain_priors:base_extractiveness(scurvy_maritime_extraction, 0.90).
% Rationale: The cure was known but actively ignored or suppressed due to cost, logistics, and competing incorrect theories.
domain_priors:suppression_score(scurvy_maritime_extraction, 0.70).
% Rationale: The constraint was brutally functional, not performative. There was little theater involved.
domain_priors:theater_ratio(scurvy_maritime_extraction, 0.10).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(scurvy_maritime_extraction, extractiveness, 0.90).
narrative_ontology:constraint_metric(scurvy_maritime_extraction, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(scurvy_maritime_extraction, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It was presented as a natural, unavoidable hazard of sea life (natural_law), but was in fact a constructed policy choice.
narrative_ontology:constraint_claim(scurvy_maritime_extraction, tangled_rope).

% Binary flags
% The naval diet was a strictly enforced policy.
domain_priors:requires_active_enforcement(scurvy_maritime_extraction).

% Structural property derivation hooks:
% Required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(scurvy_maritime_extraction, maritime_empires).
narrative_ontology:constraint_victim(scurvy_maritime_extraction, common_sailors).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE COMMON SAILOR (SNARE)
% For the sailor, the ship's diet is a lethal trap. They have no power to change it and no exit.
% The effective extraction is χ = 0.90 * 1.5 (powerless) * 1.2 (global) = 1.62, a terminal snare.
constraint_indexing:constraint_classification(scurvy_maritime_extraction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE ADMIRALTY (ROPE)
% For the empire, sailor mortality is a manageable cost in a larger coordination problem of global trade.
% Effective extraction is χ = 0.90 * -0.2 (institutional) * 1.2 (global) = -0.216, perceived as a net benefit.
constraint_indexing:constraint_classification(scurvy_maritime_extraction, rope,
    context(agent_power(institutional),
            time_horizon(historical),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees a system with a genuine coordination function (global trade)
% that is inextricably linked to asymmetric, lethal extraction, and requires active enforcement (naval policy).
% This is the canonical definition of a Tangled Rope.
constraint_indexing:constraint_classification(scurvy_maritime_extraction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(scurvy_maritime_extraction_tests).

test(perspectival_gap_snare_to_rope) :-
    constraint_indexing:constraint_classification(scurvy_maritime_extraction, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(scurvy_maritime_extraction, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(scurvy_maritime_extraction, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify the necessary conditions for a tangled_rope classification are met.
    narrative_ontology:constraint_beneficiary(scurvy_maritime_extraction, _), % derives has_coordination_function
    narrative_ontology:constraint_victim(scurvy_maritime_extraction, _),     % derives has_asymmetric_extraction
    domain_priors:requires_active_enforcement(scurvy_maritime_extraction).

test(high_extraction_threshold) :-
    domain_priors:base_extractiveness(scurvy_maritime_extraction, E),
    E >= 0.46.

:- end_tests(scurvy_maritime_extraction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The core of this model is distinguishing the biological reality (a Mountain) from the socio-political constraint (a Tangled Rope). While Vitamin C deficiency is a law of nature, the system that weaponized this law for economic gain is a human construction.
 * - Extractiveness (0.90): Set this high because the extraction was of life itself, the ultimate resource.
 * - Suppression (0.70): The cure was known for over a century before widespread adoption, indicating strong institutional suppression.
 * - Perspectival Gap: The gap is extreme. The sailor sees a lethal Snare. The Admiralty sees a Rope for coordinating empire. The analyst sees the synthesis: a Tangled Rope where coordination and extraction are two sides of the same coin. This prevents mandatrophy by correctly identifying the system's dual nature, rather than misclassifying it as pure coordination (Rope) or a pure natural hazard (Mountain).
 * [RESOLVED MANDATROPHY]
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_scurvy_intent,
    'Was the 40+ year delay between Lind''s 1753 treatise and the Royal Navy''s adoption of citrus a result of bureaucratic incompetence (a Piton-like inertia) or a deliberate cost-saving measure (a Snare)?',
    'Analysis of Admiralty budgets and correspondence from 1753-1795, comparing sailor replacement costs vs. projected cost of citrus supply.',
    'If incompetence, the system was path-dependent and inertial. If deliberate, it was predatory extraction.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(scurvy_maritime_extraction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data for high-extraction constraint (0.90 > 0.46).
% The system was brutally efficient from the start of long-range voyages.
% Theater was always low; extraction was always high.

% Theater ratio over time (consistently low):
narrative_ontology:measurement(scurvy_tr_t0, scurvy_maritime_extraction, theater_ratio, 0, 0.10).
narrative_ontology:measurement(scurvy_tr_t5, scurvy_maritime_extraction, theater_ratio, 5, 0.10).
narrative_ontology:measurement(scurvy_tr_t10, scurvy_maritime_extraction, theater_ratio, 10, 0.10).

% Extraction over time (consistently high):
narrative_ontology:measurement(scurvy_ex_t0, scurvy_maritime_extraction, base_extractiveness, 0, 0.90).
narrative_ontology:measurement(scurvy_ex_t5, scurvy_maritime_extraction, base_extractiveness, 5, 0.90).
narrative_ontology:measurement(scurvy_ex_t10, scurvy_maritime_extraction, base_extractiveness, 10, 0.90).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The system of naval provisioning and manning was a form of global infrastructure
% that enabled the projection of state power and trade.
narrative_ontology:coordination_type(scurvy_maritime_extraction, global_infrastructure).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */