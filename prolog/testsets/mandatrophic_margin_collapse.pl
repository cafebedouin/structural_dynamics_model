% ============================================================================
% CONSTRAINT STORY: mandatrophic_margin_collapse
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-27
% ============================================================================

:- module(constraint_mandatrophic_margin_collapse, []).

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
    narrative_ontology:human_readable/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: mandatrophic_margin_collapse
 * human_readable: Mandatrophy (The Extraction of Margin for Mandate)
 * domain: institutional/technological
 * * SUMMARY:
 * Mandatrophy is the systemic wasting away of resilience caused by the rigid
 * prioritization of a top-down administrative mandate over the organic
 * margins (buffers, redundancies, or "slack") required for survival.
 * As the mandate extracts the margin to achieve "efficiency," the system
 * transitions from a flexible state to a brittle one, vulnerable to collapse.
 * * KEY AGENTS:
 * - The Mandator: Institutional; sets the "KPI" or "Mandate" and views
 * all unused resources (margins) as waste.
 * - The Systemic Operator: Powerless; manages the actual friction of the
 * real world and recognizes the danger of the Snare.
 * - The Stress Event: Analytical; the external disruption that
 * reveals the Mountain of physical reality once the margin is gone.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(mandatrophic_margin_collapse, 0.85). % Mountain suppression <= 0.05, Rope extraction <= 0.15, Snare extraction >= 0.46
domain_priors:suppression_score(mandatrophic_margin_collapse, 0.70).   % Structural property (raw, unscaled). Only extractiveness is scaled (by power and scope).
domain_priors:theater_ratio(mandatrophic_margin_collapse, 0.40).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
% These mirror domain_priors values using the metric key names from config.pl.
narrative_ontology:constraint_metric(mandatrophic_margin_collapse, extractiveness, 0.85).
narrative_ontology:constraint_metric(mandatrophic_margin_collapse, suppression_requirement, 0.70).
narrative_ontology:constraint_metric(mandatrophic_margin_collapse, theater_ratio, 0.40).

% Constraint self-claim (what does the constraint claim to be?)
% Values: natural_law, coordination, constructed, enforcement
narrative_ontology:constraint_claim(mandatrophic_margin_collapse, tangled_rope).
narrative_ontology:human_readable(mandatrophic_margin_collapse, "Mandatrophy (The Extraction of Margin for Mandate)").

% Binary flags
domain_priors:requires_active_enforcement(mandatrophic_margin_collapse). % Required for Tangled Rope

% Structural property derivation hooks:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(mandatrophic_margin_collapse, quarterly_metrics).
narrative_ontology:constraint_beneficiary(mandatrophic_margin_collapse, short_term_efficiency).
narrative_ontology:constraint_victim(mandatrophic_margin_collapse, systemic_resilience).
narrative_ontology:constraint_victim(mandatrophic_margin_collapse, long_term_viability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SYSTEMIC OPERATOR (SNARE)
% High extraction felt as a predatory trap that removes the tools for survival.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE MANDATOR (ROPE)
% Viewed as an essential coordination mechanism to eliminate waste and improve efficiency.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Recognizes both the coordination claim and the severe asymmetric extraction.
% The system is a Tangled Rope: it has a coordination function (beneficiary exists),
% asymmetric extraction (victim exists), and requires active enforcement.
constraint_indexing:constraint_classification(mandatrophic_margin_collapse, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(mandatrophic_margin_collapse_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(mandatrophic_margin_collapse, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(mandatrophic_margin_collapse, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % The analytical observer should correctly identify the hybrid nature.
    constraint_indexing:constraint_classification(mandatrophic_margin_collapse, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % High extraction confirms Snare/Tangled Rope status.
    domain_priors:base_extractiveness(mandatrophic_margin_collapse, E),
    E > 0.46.

:- end_tests(mandatrophic_margin_collapse_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * [RESOLVED MANDATROPHY]
 * The core of this constraint is the perspectival gap between efficiency and resilience.
 * The Mandator (institutional) sees margin as waste, so their mandate to eliminate it
 * appears as a pure coordination 'Rope'. The Operator (powerless), who relies on that
 * margin to handle real-world friction, experiences its removal as an extractive 'Snare'
 * that increases risk and brittleness. The base extractiveness (0.85) is high because
 * the mandate consumes the system's capacity for future survival to meet present targets.
 * Suppression (0.70) is high because concepts like "slack" or "resilience engineering"
 * are actively framed as inefficient and wasteful, suppressing alternatives.
 *
 * * MANDATROPHY ANALYSIS:
 * The Analytical Observer classifies this as a 'Tangled Rope'. This is critical because
 * it prevents the system from simplifying the dynamic into a pure Snare. The Mandator's
 * goal *is* a form of coordination (aligning the system to a metric), which is why
 * `constraint_beneficiary` is declared. However, this coordination is achieved through
 * severe asymmetric extraction from the system's long-term viability, captured by
 * `constraint_victim`. The `requires_active_enforcement` flag confirms that this is not
 * a self-sustaining coordination but one that must be coercively imposed. This hybrid
 * classification correctly identifies that a seemingly rational goal can become a
 * destructive extractive mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_mandatrophic_margin_collapse,
    "Can 'margin' ever be accurately measured by the Mandator, or is it an irreducible qualitative property only visible to the Operator?",
    "Comparative analysis of quantitative vs. qualitative risk assessments in failed aerospace and infrastructure projects.",
    "If Measurable: The mandate is a flawed Rope. If Irreducible: The mandate is a structural Snare.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(mandatrophic_margin_collapse, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data enables drift detection. This models the mandate starting as a
% genuine, low-extraction coordination effort that intensifies over time,
% extracting more and more systemic resilience.
% Required for high-extraction constraints (base_extractiveness > 0.46).

% Theater ratio over time (mandate becomes more performative as real margin vanishes):
narrative_ontology:measurement(mandatrophic_margin_collapse_tr_t0, mandatrophic_margin_collapse, theater_ratio, 0, 0.10).
narrative_ontology:measurement(mandatrophic_margin_collapse_tr_t5, mandatrophic_margin_collapse, theater_ratio, 5, 0.25).
narrative_ontology:measurement(mandatrophic_margin_collapse_tr_t10, mandatrophic_margin_collapse, theater_ratio, 10, 0.40).

% Extraction over time (the mandate's "bite" increases):
narrative_ontology:measurement(mandatrophic_margin_collapse_ex_t0, mandatrophic_margin_collapse, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(mandatrophic_margin_collapse_ex_t5, mandatrophic_margin_collapse, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(mandatrophic_margin_collapse_ex_t10, mandatrophic_margin_collapse, base_extractiveness, 10, 0.85).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The mandate is a mechanism for enforcing a specific behavior or outcome.
narrative_ontology:coordination_type(mandatrophic_margin_collapse, enforcement_mechanism).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */