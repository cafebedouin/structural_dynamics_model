% ============================================================================
% CONSTRAINT STORY: bureaucratic_self_preservation
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-29
% ============================================================================

:- module(constraint_bureaucratic_self_preservation, []).

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
 * * constraint_id: bureaucratic_self_preservation
 * human_readable: The Inertial Office
 * domain: political
 * * SUMMARY:
 * A phenomenon where an administrative body prioritizes its own survival and 
 * budget growth over its original mission. It creates complex, extractive 
 * processes that serve to justify its existence, eventually outliving its 
 * utility and becoming a Piton.
 * * KEY AGENTS:
 * - The Applicant: Subject (Powerless) - Navigating dead-end paperwork.
 * - The Bureau Chief: Beneficiary (Institutional) - Maintaining headcounts and budget.
 * - The Auditor: Auditor (Analytical) - Observing the efficiency decay and theatrical compliance.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(bureaucratic_self_preservation, 0.72). % Snare extraction >= 0.46
domain_priors:suppression_score(bureaucratic_self_preservation, 0.80).   % High due to monopoly on a required service/permit.
domain_priors:theater_ratio(bureaucratic_self_preservation, 0.85).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(bureaucratic_self_preservation, extractiveness, 0.72).
narrative_ontology:constraint_metric(bureaucratic_self_preservation, suppression_requirement, 0.80).
narrative_ontology:constraint_metric(bureaucratic_self_preservation, theater_ratio, 0.85).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(bureaucratic_self_preservation, piton).
narrative_ontology:human_readable(bureaucratic_self_preservation, "The Inertial Office").

% Binary flags
domain_priors:requires_active_enforcement(bureaucratic_self_preservation).

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(bureaucratic_self_preservation, senior_bureaucrats).
narrative_ontology:constraint_victim(bureaucratic_self_preservation, public_applicants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The applicant experiences the bureaucracy as a predatory trap of red tape,
% where χ = 0.72 * 1.5 (powerless) * 1.0 (national) = 1.08.
constraint_indexing:constraint_classification(bureaucratic_self_preservation, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% The bureau chief sees the rules as essential coordination ("due process"),
% experiencing negative extraction (benefit): χ = 0.72 * -0.2 (institutional) * 1.0 (national) = -0.144.
constraint_indexing:constraint_classification(bureaucratic_self_preservation, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (PITON)
% The auditor classifies it as a Piton, where the original function has atrophied,
% evidenced by the extremely high theater_ratio (0.85), which overrides the high extraction.
constraint_indexing:constraint_classification(bureaucratic_self_preservation, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))) :-
    domain_priors:theater_ratio(bureaucratic_self_preservation, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(bureaucratic_self_preservation_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless (snare) and institutional (rope).
    constraint_indexing:constraint_classification(bureaucratic_self_preservation, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(bureaucratic_self_preservation, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(bureaucratic_self_preservation, piton, context(agent_power(analytical), _, _, _)).

test(piton_detection_by_theater) :-
    % Verify the analytical observer sees the Piton due to high theater_ratio.
    domain_priors:theater_ratio(bureaucratic_self_preservation, TR),
    TR >= 0.70,
    constraint_indexing:constraint_classification(bureaucratic_self_preservation, piton, context(agent_power(analytical), _, _, _)).

:- end_tests(bureaucratic_self_preservation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base_extractiveness is very high (0.72) because the bureaucracy absorbs 
 * significant public resources (time, fees, tax funding) relative to its stated 
 * mission's output. Suppression is high (0.80) as it's often a state-mandated 
 * monopoly for a necessary service or permit. The key feature is the extremely
 * high theater_ratio (0.85), indicating most activity is performative (e.g.,
 * generating reports, holding meetings) rather than functional.
 *
 * PERSPECTIVAL GAP:
 * The gap is stark: the applicant sees a Snare (a costly, pointless trap), while
 * the institutional beneficiary sees a Rope (a necessary coordination tool that
 * justifies their employment and budget). The analytical observer, however, uses
 * the high theater_ratio to correctly identify it as a Piton—a structure that
 * has lost its original purpose but remains due to institutional inertia.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The high extraction (0.72) could suggest a Tangled Rope or Snare. However,
 * the system resolves this ambiguity using the theater_ratio. A theater_ratio
 * of 0.85 strongly indicates that the constraint's primary function is no longer
 * coordination or extraction for an external goal, but its own self-perpetuation.
 * This correctly classifies it as a Piton, a decayed form of a previous constraint,
 * thus resolving the mandatrophy.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_bureaucratic_self_preservation,
    'Is the bureaucratic complexity a result of legislative mandate (external) or internal procedural expansion for self-preservation (internal)?',
    'An audit comparing the agency''s internal rulebook against the explicit requirements of its founding legislation.',
    'If external, the constraint is closer to a Tangled Rope created by policy. If internal, it is a pure Piton of institutional decay.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(bureaucratic_self_preservation, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This bureaucracy began as a functional entity but decayed over time,
% accumulating extraction and substituting theatrical activity for real work.
% This data is required because base_extractiveness > 0.46.

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(bsp_tr_t0, bureaucratic_self_preservation, theater_ratio, 0, 0.10).
narrative_ontology:measurement(bsp_tr_t5, bureaucratic_self_preservation, theater_ratio, 5, 0.50).
narrative_ontology:measurement(bsp_tr_t10, bureaucratic_self_preservation, theater_ratio, 10, 0.85).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(bsp_ex_t0, bureaucratic_self_preservation, base_extractiveness, 0, 0.25).
narrative_ontology:measurement(bsp_ex_t5, bureaucratic_self_preservation, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(bsp_ex_t10, bureaucratic_self_preservation, base_extractiveness, 10, 0.72).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% A bureaucracy's function is often to manage and distribute resources or permits.
narrative_ontology:coordination_type(bureaucratic_self_preservation, resource_allocation).

% Network relationships (structural influence edges)
% Inertial bureaucracies can enable other forms of institutional failure.
narrative_ontology:affects_constraint(bureaucratic_self_preservation, regulatory_capture).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */