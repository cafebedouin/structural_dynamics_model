% ============================================================================
% CONSTRAINT STORY: neurodiversity_spectrum
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-21
% ============================================================================

:- module(constraint_neurodiversity_spectrum, []).

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
 * * constraint_id: neurodiversity_spectrum
 * human_readable: The Social/Medical Model of the Neurodiversity Spectrum
 * domain: social/medical
 * * SUMMARY:
 * This constraint represents the modern social and medical model that reframes
 * neurodevelopmental variation (e.g., autism, ADHD) from a binary "normal vs.
 * abnormal" model to a spectrum. While intended to be supportive, its
 * institutionalization creates both coordination benefits (resource allocation,
 * identity) and extractive effects (pathologization, labeling). The constraint
 * is the *system*, not the underlying biological reality.
 * * KEY AGENTS:
 * - Newly Diagnosed Individual: Subject (Powerless), experiences the label as a trap.
 * - Medical Establishment (e.g., APA): Beneficiary (Institutional), uses the model for coordination.
 * - Systems Analyst: Auditor (Analytical), observes the mixed effects.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(neurodiversity_spectrum, 0.72). % Represents the extraction of autonomy and the friction of pathologization, even in a "supportive" model.
domain_priors:suppression_score(neurodiversity_spectrum, 0.55).   % The model still suppresses alternative, non-medicalized views of identity and enforces conformity to diagnostic criteria.
domain_priors:theater_ratio(neurodiversity_spectrum, 0.20).       % The system is largely functional, not theatrical.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(neurodiversity_spectrum, extractiveness, 0.72).
narrative_ontology:constraint_metric(neurodiversity_spectrum, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(neurodiversity_spectrum, theater_ratio, 0.20).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(neurodiversity_spectrum, tangled_rope).
narrative_ontology:human_readable(neurodiversity_spectrum, "The Social/Medical Model of the Neurodiversity Spectrum").
narrative_ontology:topic_domain(neurodiversity_spectrum, "social/medical").

% Binary flags
domain_priors:requires_active_enforcement(neurodiversity_spectrum). % Requires diagnosis via frameworks like the DSM-5.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(neurodiversity_spectrum, neurodivergent_community). % Gains identity, language, and access to support.
narrative_ontology:constraint_victim(neurodiversity_spectrum, individuals_pathologized_by_labeling). % Suffers from stigma, medicalization, and reduced autonomy.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE NEWLY DIAGNOSED INDIVIDUAL (SNARE)
% For an individual facing the social and professional consequences of a new
% label, the system feels like a Snare. It extracts autonomy and imposes an
% identity, trapping them in a medicalized framework.
% χ = 0.72 * 1.5 (powerless) * 1.0 (national) = 1.08
constraint_indexing:constraint_classification(neurodiversity_spectrum, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE MEDICAL ESTABLISHMENT (ROPE)
% From the perspective of an institution like the APA, the spectrum is a pure
% Rope. It's a coordination mechanism to allocate resources, define support
% levels, and create a standardized language for research and treatment.
% χ = 0.72 * -0.2 (institutional) * 1.2 (global) = -0.17
constraint_indexing:constraint_classification(neurodiversity_spectrum, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function and the asymmetric extraction.
% The system provides real benefits but also imposes significant costs on a
% victim class, and requires active enforcement to maintain. This is the
% canonical definition of a Tangled Rope.
constraint_indexing:constraint_classification(neurodiversity_spectrum, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(neurodiversity_spectrum_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(neurodiversity_spectrum, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(neurodiversity_spectrum, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional,
    TypePowerless == snare,
    TypeInstitutional == rope.

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical view correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(neurodiversity_spectrum, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    % Verify the base extraction is in the high range for Snare/Tangled Rope.
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(neurodiversity_spectrum, ExtMetricName, E),
    E >= 0.46.

:- end_tests(neurodiversity_spectrum_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The original file conflated the biological fact of neurodiversity (a Mountain)
 * with the social/medical system constructed around it. This version models only
 * the latter, revealing it as a classic Tangled Rope. The high base extraction
 * (0.72) reflects the significant cost of pathologization and loss of autonomy,
 * even when the system's intent is supportive.
 *
 * The Perspectival Gap is stark:
 * - For the institution, it's a pure coordination tool (Rope), as the negative
 *   extraction value shows they perceive no cost, only benefit.
 * - For the individual being labeled, it's a coercive trap (Snare), as the high
 *   positive extraction value shows they bear the full cost.
 * - The analytical view, accounting for both sides, correctly identifies it as
 *   a Tangled Rope.
 *
 * * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Tangled Rope classification resolves the mandatrophy by acknowledging
 * both the genuine coordination function (the Rope aspect) and the severe,
 * asymmetric extraction (the Snare aspect). It prevents the system from being
 * misclassified as either a pure Rope (ignoring the victims) or a pure Snare
 * (ignoring the real coordination benefits).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_neurodiversity_spectrum,
    "Is the historical labeling of neurodivergence as 'illness' a predatory attempt at social conformity, or a genuine (but flawed) attempt at care?",
    "Audit of historical medical outcomes vs. modern quality-of-life improvements in spectrum-based systems.",
    "If predatory: The historical 'Snare' was a tool of social control. If flawed care: The 'Snare' was a misapplied 'Rope'.",
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(neurodiversity_spectrum, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This constraint evolved from an advocacy concept into an institutionalized
% medical framework. This progression shows extraction accumulation as the
% model was formalized, creating new forms of pathologization.
%
% Theater ratio over time (slight increase with bureaucratization):
narrative_ontology:measurement(neurodiversity_spectrum_tr_t0, neurodiversity_spectrum, theater_ratio, 0, 0.10).
narrative_ontology:measurement(neurodiversity_spectrum_tr_t5, neurodiversity_spectrum, theater_ratio, 5, 0.15).
narrative_ontology:measurement(neurodiversity_spectrum_tr_t10, neurodiversity_spectrum, theater_ratio, 10, 0.20).

% Extraction over time (increases as the model becomes institutionalized):
narrative_ontology:measurement(neurodiversity_spectrum_ex_t0, neurodiversity_spectrum, base_extractiveness, 0, 0.50).
narrative_ontology:measurement(neurodiversity_spectrum_ex_t5, neurodiversity_spectrum, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(neurodiversity_spectrum_ex_t10, neurodiversity_spectrum, base_extractiveness, 10, 0.72).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% The DSM model is fundamentally a resource allocation and diagnostic tool.
narrative_ontology:coordination_type(neurodiversity_spectrum, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */