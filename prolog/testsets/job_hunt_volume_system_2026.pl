% ============================================================================
% CONSTRAINT STORY: job_hunt_volume_system_2026
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_job_hunt_volume_system_2026, []).

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
 * * constraint_id: job_hunt_volume_system_2026
 * human_readable: The Algorithmic Volume Filter in Modern Recruitment
 * domain: economic
 * * SUMMARY:
 * In the modern digital labor market, job application systems have evolved from talent identification tools
 * into high-volume management platforms. Applicant Tracking Systems (ATS) are designed to filter hundreds of
 * applications per role, creating a system where the vast majority of applicant labor (crafting resumes,
 * filling forms) is consumed with negligible human review. This creates a structural trap where following
 * the prescribed "front door" process is statistically ineffective.
 * * KEY AGENTS:
 * - Standard Applicant: The subject, interacting with the system as designed (Powerless).
 * - Corporate HR Department: The beneficiary, using the system to manage otherwise unmanageable volume (Institutional).
 * - Labor Market Analyst: The observer, analyzing the system's structure and effects (Analytical).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(job_hunt_volume_system_2026, 0.75). % Mountain <= 0.15, Rope <= 0.15, Snare >= 0.46
domain_priors:suppression_score(job_hunt_volume_system_2026, 0.60).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(job_hunt_volume_system_2026, 0.10).       % Piton detection (>= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(job_hunt_volume_system_2026, extractiveness, 0.75).
narrative_ontology:constraint_metric(job_hunt_volume_system_2026, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(job_hunt_volume_system_2026, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a necessary mechanism for enforcing quality and managing volume.
narrative_ontology:constraint_claim(job_hunt_volume_system_2026, tangled_rope).
narrative_ontology:human_readable(job_hunt_volume_system_2026, "The Algorithmic Volume Filter in Modern Recruitment").

% Binary flags
domain_priors:requires_active_enforcement(job_hunt_volume_system_2026). % The ATS and HR processes must be actively maintained.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(job_hunt_volume_system_2026, corporate_hr_departments).
narrative_ontology:constraint_victim(job_hunt_volume_system_2026, standard_applicants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   ========================================================================== */

% PERSPECTIVE 1: THE STANDARD APPLICANT (SNARE)
% Experiences the system as a trap where effort is consumed with no return.
% The "front door" is a high-coercion, low-coordination mechanism from this view.
% χ = 0.75 * π(powerless:1.5) * σ(global:1.2) = 1.35 (Extremely high extraction)
constraint_indexing:constraint_classification(job_hunt_volume_system_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE CORPORATE HR DEPARTMENT (ROPE)
% Views the system as an essential coordination tool to manage overwhelming volume.
% Without it, the hiring process would collapse.
% χ = 0.75 * π(institutional:-0.2) * σ(national:1.0) = -0.15 (Net benefit)
constraint_indexing:constraint_classification(job_hunt_volume_system_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE LABOR MARKET ANALYST (TANGLED ROPE)
% Recognizes both the coordination function for institutions and the asymmetric
% extraction from applicants. The system is a hybrid, not pure coordination or pure extraction.
% χ = 0.75 * π(analytical:1.15) * σ(global:1.2) = 1.035 (High extraction)
constraint_indexing:constraint_classification(job_hunt_volume_system_2026, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(job_hunt_volume_system_2026_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(job_hunt_volume_system_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(job_hunt_volume_system_2026, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_tangled_rope) :-
    % The analytical view must resolve the conflict by identifying the hybrid nature.
    constraint_indexing:constraint_classification(job_hunt_volume_system_2026, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    % Verify all three structural requirements for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(job_hunt_volume_system_2026, _),
    narrative_ontology:constraint_victim(job_hunt_volume_system_2026, _),
    domain_priors:requires_active_enforcement(job_hunt_volume_system_2026).

:- end_tests(job_hunt_volume_system_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The scores reflect a system that began as a solution to a scaling problem but has become
 * pathologically extractive. The base extractiveness (0.75) represents the immense waste of
 * applicant labor (e.g., 599+ applications worth of effort discarded per hire). The suppression
 * score (0.60) reflects the persistence of the "meritocracy" narrative that encourages
 * participation in a statistically futile process.
 *
 * The Perspectival Gap is stark: for HR, it's an indispensable Rope for managing volume; for
 * applicants, it's a Snare that consumes their time and hope.
 *
 * MANDATROPHY ANALYSIS: [RESOLVED MANDATROPHY]
 * The Mandatrophy is resolved by the analytical classification of Tangled Rope. A simpler
 * analysis might label the system a pure Snare, ignoring its critical (and non-trivial)
 * coordination function for employers. Conversely, calling it a Rope would ignore the
 * severe, asymmetric extraction imposed on applicants. The Tangled Rope classification
 * correctly captures this dual-nature, acknowledging both the coordination benefit and the
 * extractive harm, preventing a mischaracterization of the system's complex reality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_jhs_intent,
    'Is the extreme extraction an intended feature to filter for desperation, or an unintended consequence of poorly designed scaling tools?',
    'Internal communications and design documents from major ATS providers.',
    'If intended -> Snare by design. If unintended -> A pathologically degraded Rope.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(job_hunt_volume_system_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% This system became more extractive over time as digital application volume exploded.
% This data models the accumulation of extraction.

% Theater ratio over time (remains low as the system is brutally functional):
narrative_ontology:measurement(jhs_2026_tr_t0, job_hunt_volume_system_2026, theater_ratio, 0, 0.05).
narrative_ontology:measurement(jhs_2026_tr_t5, job_hunt_volume_system_2026, theater_ratio, 5, 0.08).
narrative_ontology:measurement(jhs_2026_tr_t10, job_hunt_volume_system_2026, theater_ratio, 10, 0.10).

% Extraction over time (models the shift from a manageable filter to a volume trap):
narrative_ontology:measurement(jhs_2026_ex_t0, job_hunt_volume_system_2026, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(jhs_2026_ex_t5, job_hunt_volume_system_2026, base_extractiveness, 5, 0.65).
narrative_ontology:measurement(jhs_2026_ex_t10, job_hunt_volume_system_2026, base_extractiveness, 10, 0.75).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: The system's primary function is to allocate the scarce
% resource of recruiter attention.
narrative_ontology:coordination_type(job_hunt_volume_system_2026, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */