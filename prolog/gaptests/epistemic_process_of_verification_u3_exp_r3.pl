% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u3_exp_r3
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u3_exp_r3, []).

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
 *   constraint_id: epistemic_process_of_verification_u3_exp_r3
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   The requirement for independent verification and replication is the
 *   central coordination mechanism of modern science. It ensures that the
 *   body of accepted knowledge is robust and reliable. However, this process
 *   is not frictionless. It imposes significant costs in time, resources, and
 *   career risk, creating a system of gatekeeping that benefits established
 *   institutions while extracting from and suppressing novel claims from
 *   less-resourced or paradigm-challenging actors.
 *
 * KEY AGENTS:
 *   - Early-career researchers / Underfunded labs: Primary targets (powerless/trapped) - bear the costs of verification.
 *   - Established research institutions / National labs: Primary beneficiaries (institutional/arbitrage) - act as gatekeepers, reinforcing their prestige.
 *   - Funding agencies / Journal publishers: Enforcers (institutional/constrained) - manage the system of verification and resource allocation.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u3_exp_r3, 0.55).
domain_priors:suppression_score(epistemic_process_of_verification_u3_exp_r3, 0.75).
domain_priors:theater_ratio(epistemic_process_of_verification_u3_exp_r3, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u3_exp_r3, extractiveness, 0.55).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u3_exp_r3, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u3_exp_r3, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u3_exp_r3, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u3_exp_r3, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u3_exp_r3, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u3_exp_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_exp_r3, established_research_institutions).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_exp_r3, funding_agencies).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_exp_r3, journal_publishers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u3_exp_r3, early_career_researchers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u3_exp_r3, underfunded_labs).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u3_exp_r3, proponents_of_paradigm_shifts).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a researcher with a novel but difficult-to-replicate finding, the process acts as a career-ending filter, suppressing their work due to lack of resources or institutional backing. It is a Snare.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For a well-funded institution that acts as a verifier, the process is a pure coordination mechanism (Rope) that maintains the integrity of the field, reinforces its own prestige, and directs research priorities.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r3, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% For a funding body, the process is a necessary tool for risk management and quality control, but they are also aware of its extractive costs and its potential to stifle high-risk, high-reward research. They experience the tension directly.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r3, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% The analytical view recognizes both the essential coordination function in building reliable knowledge and the simultaneous, structural extraction from challengers and the under-resourced. It is a canonical Tangled Rope.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r3, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u3_exp_r3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r3, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u3_exp_r3, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u3_exp_r3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness of 0.55 reflects the immense material and career costs required to satisfy verification demands, especially in capital-intensive fields. The high suppression score of 0.75 reflects the near-total delegitimization of non-replicated claims within the formal scientific enterprise; there are few viable alternative paths to epistemic authority.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: for those with the resources to verify others' work, the system is a Rope that maintains high standards. For those seeking verification for a novel claim, it is a Snare that can strangle their career. This gap is a direct function of an agent's access to capital and institutional power.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint directs value and authority towards established, well-funded institutions that can afford the high cost of verification, making them arbiters of truth. It extracts resources and potential discoveries from individuals and smaller labs who cannot meet this high bar, regardless of the intrinsic merit of their claims. The beneficiaries are the gatekeepers; the victims are the challengers.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a pure Rope would be a common error, ignoring the significant coercive and extractive elements that suppress innovation from the periphery. A Snare classification would be equally wrong, as it would deny the genuine and critical coordination function the process provides. The Tangled Rope classification correctly identifies the dual nature of the constraint: an essential coordination mechanism that has been captured to serve an extractive, gatekeeping function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_vs_suppression,
    'Is the high cost and difficulty of verification a necessary filter for quality, or a structural barrier used by incumbents to suppress paradigm-shifting innovation?',
    'Longitudinal analysis of the institutional affiliation and funding status of authors of initially-rejected-but-later-vindicated research versus authors of high-profile retractions.',
    'If primarily a quality filter, the constraint's extractiveness is overstated and it functions more like a Rope. If primarily a suppression mechanism, it functions more like a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_vs_suppression, empirical, 'Distinguishing necessary epistemic rigor from incumbent-driven suppression of novel claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u3_exp_r3, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1950, epistemic_process_of_verification_u3_exp_r3, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(epis_tr_t1985, epistemic_process_of_verification_u3_exp_r3, theater_ratio, 1985, 0.2).
narrative_ontology:measurement(epis_tr_t2025, epistemic_process_of_verification_u3_exp_r3, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(epis_be_t1950, epistemic_process_of_verification_u3_exp_r3, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(epis_be_t1985, epistemic_process_of_verification_u3_exp_r3, base_extractiveness, 1985, 0.45).
narrative_ontology:measurement(epis_be_t2025, epistemic_process_of_verification_u3_exp_r3, base_extractiveness, 2025, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u3_exp_r3, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u3_exp_r3, public_trust_in_science).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u3_exp_r3, pharmaceutical_approval_process).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u3_exp_r3, academic_tenure_system).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
