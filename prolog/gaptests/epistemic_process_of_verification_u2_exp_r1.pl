% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u2_exp_r1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u2_exp_r1, []).

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
 *   constraint_id: epistemic_process_of_verification_u2_exp_r1
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   The requirement for independent verification of scientific claims is a
 *   foundational constraint of the modern scientific method. It serves a
 *   critical coordination function, aligning the global research community on
 *   a shared body of reliable knowledge. However, this process is not
 *   frictionless. It imposes significant costs in time, resources, and
 *   reputational risk, which are disproportionately borne by the individuals
 *   and teams proposing novel, paradigm-challenging claims.
 *
 * KEY AGENTS:
 *   - Novel Claimants: Researchers proposing new findings (powerless/trapped).
 *   - Established Scientific Community: Journal editors, peer reviewers, senior academics who act as gatekeepers (institutional/arbitrage).
 *   - Funding Agencies & The Public: The ultimate sponsors and beneficiaries of reliable knowledge (organized/constrained).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u2_exp_r1, 0.48).
domain_priors:suppression_score(epistemic_process_of_verification_u2_exp_r1, 0.75).
domain_priors:theater_ratio(epistemic_process_of_verification_u2_exp_r1, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r1, extractiveness, 0.48).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r1, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r1, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u2_exp_r1, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u2_exp_r1, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u2_exp_r1, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u2_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r1, established_scientific_community).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r1, funding_agencies).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r1, the_public).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r1, novel_claimants).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r1, researchers_at_low_prestige_institutions).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the individual researcher with a groundbreaking but unverified claim, the system is a high-cost, high-suppression gauntlet. Their career, funding, and reputation are at risk. The coordination benefit is an externality to their immediate struggle.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% From the perspective of those who uphold the standards, this is a pure coordination mechanism to filter signal from noise and maintain the integrity of the scientific record. The costs imposed on claimants are seen as a necessary price for rigor.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r1, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the essential coordination function (creating reliable knowledge) and the asymmetric extraction (the heavy burden placed on innovators, which can sometimes stifle progress).
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% Society and its funding bodies see the process as a temporary structure to produce a specific outcome: reliable facts. They tolerate the internal costs and inefficiencies in expectation of a technological or social payoff. The 'sunset' is the eventual validation or rejection of a given claim.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r1, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u2_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r1, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u2_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u2_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.48) reflects the high cost of novel research and the risk of being 'scooped' or dismissed during the lengthy verification period. The suppression score (0.75) is high because the system is explicitly designed to suppress unverified claims from entering the canon; operating outside this system is nearly impossible for a practicing scientist.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Novel Claimant, who experiences the process as a potentially career-ending Snare, and the Established Institution, which views it as a necessary Rope for maintaining quality control. The analytical perspective of Tangled Rope reconciles these by acknowledging both functions are simultaneously present.
 *
 * DIRECTIONALITY LOGIC:
 *   The direction of extraction is from the innovator towards the established paradigm. Novel Claimants and researchers at less-prestigious institutions (the victims) pay the costs of verification. The Established Community and the Public (the beneficiaries) receive the benefit of a stable, reliable knowledge base. This structural asymmetry is the core of the Tangled Rope classification.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a pure Rope would be a form of mandatrophy, ignoring the real and substantial costs extracted from innovators. Conversely, classifying it as a pure Snare would ignore its undeniably critical and successful coordination function. The Tangled Rope classification correctly identifies that the system has both a genuine coordination purpose and an asymmetric extractive cost structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_paradigm_lock_in,
    'Does the verification process ossify into a defense of the existing paradigm, actively preventing revolutionary discoveries, or is it a reasonably neutral filter for truth?',
    'Historical analysis of Kuhnian paradigm shifts, comparing the time-to-acceptance for paradigm-confirming vs. paradigm-challenging results.',
    'If it primarily functions as a defense mechanism, its classification shifts towards Snare. If it is a neutral filter, it is closer to a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_paradigm_lock_in, empirical, 'Whether the process is a neutral filter or a paradigm defense mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u2_exp_r1, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1950, epistemic_process_of_verification_u2_exp_r1, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(epis_tr_t1990, epistemic_process_of_verification_u2_exp_r1, theater_ratio, 1990, 0.15).
narrative_ontology:measurement(epis_tr_t2024, epistemic_process_of_verification_u2_exp_r1, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(epis_be_t1950, epistemic_process_of_verification_u2_exp_r1, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement(epis_be_t1990, epistemic_process_of_verification_u2_exp_r1, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(epis_be_t2024, epistemic_process_of_verification_u2_exp_r1, base_extractiveness, 2024, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u2_exp_r1, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
