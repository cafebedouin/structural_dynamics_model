% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u4_exp_r1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-29
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u4_exp_r1, []).

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
 *   constraint_id: epistemic_process_of_verification_u4_exp_r1
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   This constraint represents the standard scientific method of requiring
 *   independent replication before a novel claim is accepted. While it serves
 *   a crucial coordination function by creating a shared, reliable body of
 *   knowledge, it also imposes significant costs (time, funding, career risk)
 *   that are asymmetrically borne by new entrants and those challenging
 *   established paradigms.
 *
 * KEY AGENTS:
 *   - Early-Career Researchers / Paradigm Challengers: Primary victims (powerless/trapped) who bear the burden of proof.
 *   - Scientific Establishment (Universities, Journals, Funding Agencies): Primary beneficiaries (institutional/arbitrage) who act as gatekeepers and benefit from the system's stability.
 *   - Established Senior Scientists: Secondary beneficiaries (powerful/mobile) who have successfully navigated the system.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u4_exp_r1, 0.55).
domain_priors:suppression_score(epistemic_process_of_verification_u4_exp_r1, 0.65).
domain_priors:theater_ratio(epistemic_process_of_verification_u4_exp_r1, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u4_exp_r1, extractiveness, 0.55).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u4_exp_r1, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u4_exp_r1, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u4_exp_r1, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u4_exp_r1, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u4_exp_r1, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u4_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u4_exp_r1, scientific_establishment).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u4_exp_r1, funding_agencies).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u4_exp_r1, journal_publishers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u4_exp_r1, early_career_researchers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u4_exp_r1, underfunded_labs).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u4_exp_r1, paradigm_challengers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a researcher with a novel but difficult-to-replicate finding, the process is a high-risk gauntlet where failure means career death, making it a Snare.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% For the establishment, this is a pure coordination mechanism (Rope) that ensures the stability and reliability of the scientific corpus, justifying the costs as quality control.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r1, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the essential coordination function and the asymmetric extraction imposed on new entrants, classifying it as a Tangled Rope.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% A successful, established academic perceives the system as a functional Rope, having already paid the entry costs and now benefiting from its stability and prestige.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r1, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u4_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r1, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u4_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u4_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) reflects the high, often uncompensated, cost of replication and the career-ending risk for those whose novel results are not immediately reproduced. The suppression score (0.65) reflects the system's strong filtering effect against claims that are true but resource-intensive to verify, effectively removing them from consideration and narrowing the path of inquiry.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the 'powerless' challenger, who experiences the process as a high-stakes Snare, and the 'institutional' gatekeeper, who views it as a necessary Rope for maintaining quality control. The challenger is trapped within the system, while the institution has arbitrage over which claims it chooses to engage with.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the flow of resources and risk. Beneficiaries (the establishment) provide the platform but harvest the rewards of verified knowledge (prestige, funding cycles, publications) while externalizing the highest risk of failure onto the victims (the challengers). The process extracts labor and intellectual risk from the periphery to solidify the core.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a Tangled Rope avoids two errors. It is not a pure Rope, as this would ignore the severe, asymmetric costs imposed on challengers. It is not a pure Snare, as this would deny its undeniably effective and vital function in coordinating the scientific community to produce reliable knowledge. The Tangled Rope classification correctly captures this essential duality of productive coordination achieved via asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    replication_cost_vs_rigor,
    'Is the high financial and temporal cost of replication a necessary feature for ensuring scientific rigor, or has it become an excessive barrier that primarily serves to entrench existing paradigms?',
    'Analysis of breakthrough rates versus the increasing capital cost of instrumentation and concentration of funding in established labs.',
    'If costs are deemed a necessary component of rigor, the constraint leans towards Rope. If they are primarily a gatekeeping mechanism, it leans towards Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(replication_cost_vs_rigor, empirical, 'The ambiguity between necessary rigor and excessive gatekeeping cost in scientific verification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u4_exp_r1, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1950, epistemic_process_of_verification_u4_exp_r1, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(epis_tr_t1990, epistemic_process_of_verification_u4_exp_r1, theater_ratio, 1990, 0.22).
narrative_ontology:measurement(epis_tr_t2024, epistemic_process_of_verification_u4_exp_r1, theater_ratio, 2024, 0.3).

% Extraction over time
narrative_ontology:measurement(epis_be_t1950, epistemic_process_of_verification_u4_exp_r1, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(epis_be_t1990, epistemic_process_of_verification_u4_exp_r1, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(epis_be_t2024, epistemic_process_of_verification_u4_exp_r1, base_extractiveness, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u4_exp_r1, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u4_exp_r1, public_trust_in_science).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u4_exp_r1, technological_development_rate).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
