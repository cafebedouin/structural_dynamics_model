% ============================================================================
% CONSTRAINT STORY: edelman_2026_insularity
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-23
% ============================================================================

:- module(constraint_edelman_2026_insularity, []).

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
 * * constraint_id: edelman_2026_insularity
 * human_readable: The Insular Trust Mindset
 * domain: social/economic
 * * SUMMARY:
 * A global condition where a majority of individuals are unwilling or hesitant to trust
 * those with different values, sources, or backgrounds. This insularity
 * acts as a "Snare" for the powerless by limiting economic mobility and
 * as a "Rope" for institutions attempting to broker local stability.
 * * KEY AGENTS:
 * - Low-Income Citizen: Subject (Powerless) - Faces a 15pt trust gap vs high income[cite: 263].
 * - Multi-national Employer: Beneficiary (Institutional) - Seen as the most trusted
 * entity to bridge divides[cite: 826, 972].
 * - Edelman Analyst: Auditor (Analytical) - Measures the erosion of shared reality[cite: 72].
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.55) reflecting lost productivity (34%) and workplace conflict[cite: 624, 630].
domain_priors:base_extractiveness(edelman_2026_insularity, 0.55).
% High suppression (0.65) as 71% of low-income fear being left behind by AI[cite: 408, 434].
domain_priors:suppression_score(edelman_2026_insularity, 0.65).
% Theater ratio (0.42) reflecting significant but not yet dominant performative "Trust Brokering"[cite: 835].
domain_priors:theater_ratio(edelman_2026_insularity, 0.42).

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(edelman_2026_insularity, extractiveness, 0.55).
narrative_ontology:constraint_metric(edelman_2026_insularity, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(edelman_2026_insularity, theater_ratio, 0.42).

% Constraint self-claim (what does the constraint claim to be?)
% It presents as a natural social condition but is a constructed reality.
narrative_ontology:constraint_claim(edelman_2026_insularity, tangled_rope).

% Binary flags
% Enforcement is social and economic (e.g., hiring bias, social exclusion), which is active.
domain_priors:requires_active_enforcement(edelman_2026_insularity). % Required for Tangled Rope

% Structural property derivation hooks:
% has_coordination_function/1 is DERIVED from constraint_beneficiary/2
% has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(edelman_2026_insularity, multi_national_employers).
narrative_ontology:constraint_victim(edelman_2026_insularity, low_income_citizens).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The low-income subject feels trapped by a system they believe favors the rich[cite: 667, 1385].
constraint_indexing:constraint_classification(edelman_2026_insularity, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Employers view the constraint as a coordination challenge to be managed via
% "Trust Brokering" and shared identity[cite: 778, 890].
constraint_indexing:constraint_classification(edelman_2026_insularity, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid nature of insularity: it provides local safety but extracts
% global progress[cite: 584, 595].
constraint_indexing:constraint_classification(edelman_2026_insularity, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(edelman_2026_insularity_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(edelman_2026_insularity, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(edelman_2026_insularity, rope, context(agent_power(institutional), _, _, _)).

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(edelman_2026_insularity, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(edelman_2026_insularity, E),
    E >= 0.46.

:- end_tests(edelman_2026_insularity_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The 0.55 extraction score is grounded in the "Loss of Productivity" metric (34%)
 * where employees put less effort into teams led by those with different beliefs[cite: 624, 630].
 * The Perspectival Gap exists because institutions see insularity as a problem
 * to be solved through "polynational" models[cite: 965], which they can afford to implement,
 * viewing it as a coordination challenge (Rope). The powerless experience it as a
 * "rigged system"[cite: 132] that limits their mobility and opportunities (Snare).
 *
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is critical here. A simpler model might classify this
 * as a pure Snare, focusing only on the harm to low-income citizens. However, that
 * would miss the genuine coordination function that "Trust Brokering" provides for
 * multi-national employers, allowing them to maintain operational stability in a
 * polarized world. Tangled Rope correctly identifies both the coordination benefit
 * for the powerful and the asymmetric extraction from the powerless, preventing
 * misclassification and providing a more accurate model of the system's dynamics.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_edelman_2026,
    'Is insularity a psychological defense (Mountain) or a manufactured state (Snare)?',
    'Analysis of the 11pt rise in fears of foreign disinformation[cite: 465, 478].',
    'If defense, trust brokering succeeds; if manufactured, insularity is an extraction tool.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(edelman_2026_insularity, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection (metric_substitution,
% extraction_accumulation) by providing measurements at multiple time points.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time: Rising as "Trust Brokering" becomes a corporate mandate[cite: 826].
narrative_ontology:measurement(ed_tr_t0, edelman_2026_insularity, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ed_tr_t5, edelman_2026_insularity, theater_ratio, 5, 0.35).
narrative_ontology:measurement(ed_tr_t10, edelman_2026_insularity, theater_ratio, 10, 0.42).

% Extraction over time: Increasing as shared reality erodes over 20 years[cite: 72].
narrative_ontology:measurement(ed_ex_t0, edelman_2026_insularity, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(ed_ex_t5, edelman_2026_insularity, base_extractiveness, 5, 0.48).
narrative_ontology:measurement(ed_ex_t10, edelman_2026_insularity, base_extractiveness, 10, 0.55).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% "Trust Brokering" by employers is a form of social resource allocation.
narrative_ontology:coordination_type(edelman_2026_insularity, resource_allocation).

% Network relationships (structural influence edges)
% Widespread social insularity directly fuels and is fueled by political polarization.
narrative_ontology:affects_constraint(edelman_2026_insularity, political_polarization_2026).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */