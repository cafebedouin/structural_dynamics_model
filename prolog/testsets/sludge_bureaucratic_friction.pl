% ============================================================================
% CONSTRAINT STORY: sludge_bureaucratic_friction
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_sludge_bureaucratic_friction, []).

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
 * * constraint_id: sludge_bureaucratic_friction
 * human_readable: Sludge (Intentional Administrative Friction)
 * domain: political/economic
 * * SUMMARY:
 * "Sludge" describes intentional administrative burdens—excessive paperwork, long wait times, complex requirements—that create friction, discouraging access to benefits, rights, or services. It functions as a non-obvious barrier, extracting time, cognitive load, and psychological well-being from applicants, often to control costs or limit participation without explicitly denying access.
 * * KEY AGENTS:
 * - The Applicant: Subject (Powerless) attempting to access a benefit (e.g., social services, rebates).
 * - The Institution: Beneficiary (Institutional) that saves resources or controls demand via the friction.
 * - The Policy Analyst: Auditor (Analytical) observing the system's dual function of providing and throttling access.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(sludge_bureaucratic_friction, 0.70). % High; extracts time, effort, and opportunity cost.
domain_priors:suppression_score(sludge_bureaucratic_friction, 0.60).   % Moderate-high; alternatives are suppressed by making the official channel prohibitively costly.
domain_priors:theater_ratio(sludge_bureaucratic_friction, 0.10).       % Low; the friction is highly functional for its purpose (throttling), not performative.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(sludge_bureaucratic_friction, extractiveness, 0.70).
narrative_ontology:constraint_metric(sludge_bureaucratic_friction, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(sludge_bureaucratic_friction, theater_ratio, 0.10).

% Constraint self-claim (what does the constraint claim to be?)
% It claims to be a necessary process for due diligence and fraud prevention.
narrative_ontology:constraint_claim(sludge_bureaucratic_friction, tangled_rope).
narrative_ontology:human_readable(sludge_bureaucratic_friction, "Sludge (Intentional Administrative Friction)").

% Binary flags
domain_priors:requires_active_enforcement(sludge_bureaucratic_friction). % Required for Tangled Rope. The rules must be actively maintained.

% Structural property derivation hooks:
% Both beneficiary and victim are required for Tangled Rope.
narrative_ontology:constraint_beneficiary(sludge_bureaucratic_friction, budget_controlling_institutions).
narrative_ontology:constraint_victim(sludge_bureaucratic_friction, resource_constrained_applicants).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   ========================================================================== */

% PERSPECTIVE 1: THE APPLICANT (SNARE)
% For someone needing a benefit, the friction is a trap that drains their
% limited resources (time, energy, money) with no guarantee of success.
constraint_indexing:constraint_classification(sludge_bureaucratic_friction, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% PERSPECTIVE 2: THE INSTITUTION (ROPE)
% From the institution's view, the process is a necessary coordination tool
% (a Rope) to manage demand, ensure compliance, and prevent fraud, thereby
% preserving the integrity and budget of the program.
constraint_indexing:constraint_classification(sludge_bureaucratic_friction, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both functions: a genuine coordination goal (managing a
% program) intertwined with asymmetric extraction (cost-shifting onto the
% most vulnerable). It requires active enforcement to maintain this balance.
constraint_indexing:constraint_classification(sludge_bureaucratic_friction, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(sludge_bureaucratic_friction_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(sludge_bureaucratic_friction, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(sludge_bureaucratic_friction, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    assertion(TypePowerless == snare),
    assertion(TypeInstitutional == rope),
    assertion(TypePowerless \= TypeInstitutional).

test(analytical_classification_is_tangled_rope) :-
    constraint_indexing:constraint_classification(sludge_bureaucratic_friction, tangled_rope, context(agent_power(analytical), _, _, _)).

test(threshold_validation_for_high_extraction) :-
    narrative_ontology:constraint_metric(sludge_bureaucratic_friction, extractiveness, E),
    assertion(E >= 0.46).

:- end_tests(sludge_bureaucratic_friction_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extractiveness (0.70) is high because sludge's primary function is to
 * extract non-monetary resources (time, cognitive load, emotional energy) from
 * applicants, which translates to monetary savings for the institution. The
 * suppression score (0.60) reflects that while alternative paths are not
 * legally forbidden, the sludge makes the official path so costly that it
 * effectively suppresses access.
 *
 * The Perspectival Gap is stark:
 * - The Applicant (powerless, trapped) experiences this as a pure Snare. The
 *   process is the punishment.
 * - The Institution (institutional, mobile) views it as a Rope, a necessary
 *   coordination mechanism for fiscal responsibility and program integrity.
 *
 * * MANDATROPHY ANALYSIS:
 * The Analytical classification is Tangled Rope. This is critical because it
 * prevents misclassification as a pure Snare. A pure Snare has no redeeming
 * coordination function. Sludge, however, is built upon a legitimate
 * coordination need (vetting applicants, managing a budget), but this need is
 * used to justify a highly extractive and asymmetric implementation. The
 * Tangled Rope classification correctly identifies this duality.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_sludge_bureaucratic_friction,
    'Is the high level of friction an intentional design for cost-control (Tangled Rope) or an emergent property of unmanaged institutional complexity and risk aversion (Piton)?',
    'Internal policy memos, budget allocation models, and records of simplification initiatives being rejected.',
    'If intentional, it is a Tangled Rope. If emergent and unmanaged, it is a system drifting towards being a Piton, where the original function is lost to inertial friction.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(sludge_bureaucratic_friction, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data models the intensification of sludge over time, as initial
% due-diligence requirements accrete into significant barriers. This is a
% classic extraction_accumulation drift pattern.
%
% Theater ratio over time (remains low and functional):
narrative_ontology:measurement(sbf_tr_t0, sludge_bureaucratic_friction, theater_ratio, 0, 0.10).
narrative_ontology:measurement(sbf_tr_t5, sludge_bureaucratic_friction, theater_ratio, 5, 0.10).
narrative_ontology:measurement(sbf_tr_t10, sludge_bureaucratic_friction, theater_ratio, 10, 0.10).

% Extraction over time (increases as more friction is added):
narrative_ontology:measurement(sbf_ex_t0, sludge_bureaucratic_friction, base_extractiveness, 0, 0.48).
narrative_ontology:measurement(sbf_ex_t5, sludge_bureaucratic_friction, base_extractiveness, 5, 0.62).
narrative_ontology:measurement(sbf_ex_t10, sludge_bureaucratic_friction, base_extractiveness, 10, 0.70).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: Sludge is a mechanism for throttling resource allocation.
narrative_ontology:coordination_type(sludge_bureaucratic_friction, resource_allocation).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */