% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u2_sed_r5
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u2_sed_r5, []).

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
 *   constraint_id: epistemic_process_of_verification_u2_sed_r5
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   The process of scientific verification requires independent replication
 *   for a claim to be accepted. This serves a critical coordination function,
 *   creating a shared, reliable body of knowledge. However, it also functions
 *   as a powerful gatekeeping mechanism, imposing significant time, resource,
 *   and career costs on those with novel or paradigm-challenging claims,
 *   while reinforcing the authority and stability of the existing scientific
 *   establishment.
 *
 * KEY AGENTS:
 *   - Paradigm Challengers (junior researchers, outsiders): Primary targets (powerless/trapped) - bear the costs of verification.
 *   - Established Gatekeepers (senior researchers, journal editors, funding agencies): Primary beneficiaries (institutional/arbitrage) - control the process and benefit from its stability.
 *   - Mid-Career Scientists: Observers/participants (moderate/mobile) - experience both the benefits of coordination and the costs of extraction.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u2_sed_r5, 0.55).
domain_priors:suppression_score(epistemic_process_of_verification_u2_sed_r5, 0.7).
domain_priors:theater_ratio(epistemic_process_of_verification_u2_sed_r5, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_sed_r5, extractiveness, 0.55).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_sed_r5, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_sed_r5, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u2_sed_r5, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u2_sed_r5, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u2_sed_r5, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u2_sed_r5).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_sed_r5, established_researchers).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_sed_r5, journal_publishers).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_sed_r5, funding_agencies).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_sed_r5, junior_researchers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_sed_r5, paradigm_challengers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a junior researcher with a novel claim, the process feels like a snare; their career and funding are hostage to a slow, costly, and potentially biased verification process controlled by incumbents.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r5, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For a journal editor or senior academic, the process is a rope that coordinates the entire field, maintains standards, and ensures the reliability of the scientific record. The extractive costs are seen as necessary for quality control.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r5, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a moderate position, the dual nature is apparent. The process is necessary for credibility (coordination) but also a significant hurdle that favors incrementalism and extracts a high price for novel contributions (extraction).
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r5, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The analytical observer sees the full structure: an essential coordination mechanism for generating reliable knowledge that is simultaneously a system of social control and resource allocation with clear winners and losers.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r5, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u2_sed_r5_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r5, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r5, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u2_sed_r5, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u2_sed_r5_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is high due to the immense career, time, and funding costs extracted from innovators, which are converted into stability and prestige for the established system. Suppression (0.70) is high because there is no viable alternative to this process within mainstream science; exit is equivalent to leaving the profession. The theater ratio (0.30) is moderate, acknowledging performative aspects like citation metrics and p-hacking, but the core function remains substantial.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: challengers see a 'snare' that traps their careers, while institutional gatekeepers see a 'rope' essential for maintaining quality. This difference arises directly from their structural positions. The gatekeeper experiences the system as a tool for coordination they wield, while the challenger experiences it as an external, coercive barrier.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the established scientific community (researchers, publishers, funders) who gain from the stability, predictability, and control over the flow of information and prestige. Victims are the junior and non-consensus researchers who must pay the high entry cost for new ideas, effectively subsidizing the system's stability with their time and career risk.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a canonical tangled rope. To label it a pure 'rope' would be to adopt the beneficiary's perspective and ignore the asymmetric extraction that can stifle innovation. To label it a pure 'snare' would be to adopt the victim's perspective and ignore the undeniable and essential coordination function it provides for science. The tangled rope classification correctly identifies that both functions are present and structurally intertwined.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_efficiency_vs_suppression,
    'Is the high cost and long delay of verification a necessary feature for ensuring rigor, or is it an inefficient and excessive barrier that primarily serves to suppress paradigm shifts?',
    'Comparative analysis of verification times, costs, and success rates across fields with different replication standards and funding models.',
    'If the cost is a necessary feature for rigor, the constraint leans more towards a costly but functional rope. If it's primarily an inefficient barrier, it leans more towards a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_efficiency_vs_suppression, empirical, 'Ambiguity between necessary rigor and inefficient suppression in the scientific verification process.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u2_sed_r5, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1970, epistemic_process_of_verification_u2_sed_r5, theater_ratio, 1970, 0.15).
narrative_ontology:measurement(epis_tr_t1995, epistemic_process_of_verification_u2_sed_r5, theater_ratio, 1995, 0.25).
narrative_ontology:measurement(epis_tr_t2020, epistemic_process_of_verification_u2_sed_r5, theater_ratio, 2020, 0.3).

% Extraction over time
narrative_ontology:measurement(epis_be_t1970, epistemic_process_of_verification_u2_sed_r5, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(epis_be_t1995, epistemic_process_of_verification_u2_sed_r5, base_extractiveness, 1995, 0.45).
narrative_ontology:measurement(epis_be_t2020, epistemic_process_of_verification_u2_sed_r5, base_extractiveness, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u2_sed_r5, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
