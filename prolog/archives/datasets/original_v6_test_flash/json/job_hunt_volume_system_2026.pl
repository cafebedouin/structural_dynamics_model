% ============================================================================
% CONSTRAINT STORY: job_hunt_volume_system_2026
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-02-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_job_hunt_volume_system_2026, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:omega_variable/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: job_hunt_volume_system_2026
 *   human_readable: The Algorithmic Volume Filter in Modern Recruitment
 *   domain: economic
 *
 * SUMMARY:
 *   In the modern digital labor market, job application systems have evolved
 *   from talent identification tools into high-volume management platforms.
 *   Algorithmic volume filters are used by companies to manage the influx of
 *   online applications. This system, while intended to streamline the hiring
 *   process, creates a structural constraint, shaping the experiences of job
 *   seekers and corporations.
 *
 * KEY AGENTS:
 *   - job_seekers: Primary target (powerless/trapped) - subject to algorithmic filtering with little control.
 *   - large_corporations: Primary beneficiary (institutional/arbitrage) - benefits from efficient volume management.
 *   - hr_software_vendors: Secondary beneficiary (powerful/mobile) - profits from system deployment and maintenance
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(job_hunt_volume_system_2026, 0.65).
domain_priors:suppression_score(job_hunt_volume_system_2026, 0.7).
domain_priors:theater_ratio(job_hunt_volume_system_2026, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(job_hunt_volume_system_2026, extractiveness, 0.65).
narrative_ontology:constraint_metric(job_hunt_volume_system_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(job_hunt_volume_system_2026, theater_ratio, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(job_hunt_volume_system_2026, tangled_rope).
narrative_ontology:human_readable(job_hunt_volume_system_2026, "The Algorithmic Volume Filter in Modern Recruitment").
narrative_ontology:topic_domain(job_hunt_volume_system_2026, "economic").

domain_priors:requires_active_enforcement(job_hunt_volume_system_2026).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(job_hunt_volume_system_2026, large_corporations).
narrative_ontology:constraint_beneficiary(job_hunt_volume_system_2026, hr_software_vendors).
narrative_ontology:constraint_victim(job_hunt_volume_system_2026, job_seekers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a job seeker, the algorithmic filtering acts as a snare, trapping them in a cycle of application without meaningful feedback. They have little to no ability to exit this system.
constraint_indexing:constraint_classification(job_hunt_volume_system_2026, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% From the perspective of large corporations, the system functions as a rope, efficiently managing the high volume of applications. It allows them to quickly filter candidates based on pre-defined criteria, reducing the time and resources spent on manual screening. They can arbitrage different software vendors.
constraint_indexing:constraint_classification(job_hunt_volume_system_2026, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% An analytical observer sees the system as a Tangled Rope: it provides a coordination function for large corporations, but it also extracts significant value from job seekers by forcing them to tailor their applications to narrowly defined algorithmic criteria. Active enforcement is subtle but present in the implicit mandate for job seekers to use specific keywords and formats.
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
    constraint_indexing:constraint_classification(job_hunt_volume_system_2026, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(job_hunt_volume_system_2026, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(job_hunt_volume_system_2026, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(job_hunt_volume_system_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness is 0.65 because job seekers are forced to expend significant effort tailoring their resumes and applications to narrowly defined algorithmic criteria, with no guarantee of success. Suppression is 0.70 because the system limits job seekers' access to opportunities, favoring those who conform to algorithmic preferences. The theater ratio is 0.40 because while the system appears efficient, much of the filtering is based on superficial criteria that do not accurately assess candidates' qualifications.
 *
 * PERSPECTIVAL GAP:
 *   The job seeker experiences the system as a snare, with limited exit options and high extraction. Large corporations, on the other hand, view it as a rope, facilitating efficient management of applications. The analytical observer sees the tangled rope, recognizing both the coordination benefits for corporations and the extraction costs for job seekers.
 *
 * DIRECTIONALITY LOGIC:
 *   Job seekers (powerless/trapped) have a high directionality value (approaching 1.0) due to their limited power and exit options, making them primary targets of extraction. Large corporations (institutional/arbitrage) have a low directionality value (approaching 0.0) as they benefit from the system's coordination function and can arbitrage different software vendors.
 *
 * MANDATROPHY ANALYSIS:
 *   The system might be mislabeled a rope if one only considers the coordination function for large corporations. However, the extraction and suppression experienced by job seekers, combined with the analytical observer's perspective, reveals the tangled rope and underlying snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    algorithm_transparency,
    'To what extent are the filtering algorithms transparent and accountable?',
    'Legal mandates for algorithm explainability; independent audits of hiring algorithms.',
    'If algorithms are transparent, the constraint shifts towards a more equitable tangled rope. If opaque, it remains a snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(algorithm_transparency, empirical, 'Transparency and accountability of filtering algorithms').

omega_variable(
    skill_representation,
    'How accurately do algorithms capture the full range of skills and qualifications relevant to a job?',
    'Comparative analysis of algorithm-selected candidates versus human-selected candidates; qualitative analysis of missed opportunities.',
    'If skills are accurately represented, the system provides genuine coordination benefit. If not, it exacerbates extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(skill_representation, empirical, 'Accuracy of algorithmic skill representation').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(job_hunt_volume_system_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(job__tr_t0, job_hunt_volume_system_2026, theater_ratio, 0, 0.2).
narrative_ontology:measurement(job__tr_t5, job_hunt_volume_system_2026, theater_ratio, 5, 0.3).
narrative_ontology:measurement(job__tr_t10, job_hunt_volume_system_2026, theater_ratio, 10, 0.4).

% Extraction over time
narrative_ontology:measurement(job__be_t0, job_hunt_volume_system_2026, base_extractiveness, 0, 0.4).
narrative_ontology:measurement(job__be_t5, job_hunt_volume_system_2026, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(job__be_t10, job_hunt_volume_system_2026, base_extractiveness, 10, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
