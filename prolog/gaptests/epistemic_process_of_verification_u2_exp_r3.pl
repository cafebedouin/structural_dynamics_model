% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u2_exp_r3
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u2_exp_r3, []).

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
 *   constraint_id: epistemic_process_of_verification_u2_exp_r3
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   The process of scientific verification requires independent replication
 *   before a claim is accepted. This serves a vital coordination function,
 *   aligning the scientific community on a shared body of reliable knowledge.
 *   However, it also imposes immense costs in time, funding, and career risk,
 *   which are asymmetrically borne by those proposing novel or disruptive
 *   findings. This dual nature makes it a classic Tangled Rope.
 *
 * KEY AGENTS:
 *   - Novel Claim Proposers: Primary targets who bear the burden of proof (organized/constrained).
 *   - Established Paradigm Holders: Primary beneficiaries who act as gatekeepers and benefit from stability (institutional/arbitrage).
 *   - Replicating Labs / Early Career Researchers: Bear the opportunity and direct costs of verification work (powerless/trapped).
 *   - The Scientific Community: A secondary beneficiary that receives a reliable knowledge base.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u2_exp_r3, 0.48).
domain_priors:suppression_score(epistemic_process_of_verification_u2_exp_r3, 0.65).
domain_priors:theater_ratio(epistemic_process_of_verification_u2_exp_r3, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r3, extractiveness, 0.48).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r3, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r3, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u2_exp_r3, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u2_exp_r3, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u2_exp_r3, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u2_exp_r3).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r3, scientific_community_at_large).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r3, established_paradigm_holders).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r3, journal_publishers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r3, novel_claim_proposers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r3, replicating_labs).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r3, early_career_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the student whose career depends on replicating a difficult result, the process is a high-stakes, high-cost trap with little personal upside in terms of prestige.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r3, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

% From the perspective of a lab proposing a groundbreaking but difficult-to-replicate finding, the process feels like a snare due to the immense resource cost, career risk, and potential for gatekeeping by established rivals.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r3, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% For a tenured professor, journal editor, or funding body, the process is a pure coordination mechanism (Rope) that maintains high standards, ensures stability, and filters out noise, protecting the integrity of their field.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r3, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the essential coordination function and the asymmetric extraction. It is a Tangled Rope, a necessary filter that nonetheless imposes heavy costs and can be captured by incumbent paradigms.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r3, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u2_exp_r3_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r3, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r3, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u2_exp_r3, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u2_exp_r3_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.48) reflects the significant resource and career costs required to pass through the verification filter. Suppression (0.65) is high because the process's explicit function is to filter out and reject uncorroborated claims, leaving no alternative path to canonization. The process requires active enforcement through peer review, editorial decisions, and funding allocation.
 *
 * PERSPECTIVAL GAP:
 *   A stark gap exists between the Established Paradigm Holder, who sees a pure Rope for maintaining quality, and the Novel Claimant, who experiences a Snare of high costs, career risk, and potential gatekeeping. The analytical perspective reconciles these by identifying the system as a Tangled Rope, acknowledging both its essential coordination role and its extractive, suppressive properties.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint extracts resources (funding, lab time, opportunity cost) and career-potential from challengers and replicators. It delivers benefits in the form of epistemic stability and reliability to the community as a whole, and disproportionately to incumbent leaders of a field who control the verification infrastructure (journals, funding panels).
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a Tangled Rope correctly avoids two major errors. It is not a pure Rope, which would ignore the immense, asymmetrically distributed costs. It is also not a pure Snare, which would deny its indispensable function in producing reliable knowledge. The classification captures the reality of a system that is both functional and extractive.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gatekeeping_vs_quality_control,
    'Is the high cost and difficulty of verification a necessary feature for ensuring quality, or has it been inflated by established paradigms to serve as a gatekeeping mechanism against disruptive ideas?',
    'Comparative analysis of replication success rates and funding outcomes for paradigm-challenging vs. paradigm-confirming research, controlling for methodological rigor.',
    'If primarily gatekeeping, the Snare characteristics are stronger (higher ε). If a necessary cost of quality, the Rope characteristics are stronger (lower ε).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_vs_quality_control, empirical, 'Distinguishing between necessary quality control and inertial paradigm gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u2_exp_r3, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1950, epistemic_process_of_verification_u2_exp_r3, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(epis_tr_t1990, epistemic_process_of_verification_u2_exp_r3, theater_ratio, 1990, 0.25).
narrative_ontology:measurement(epis_tr_t2025, epistemic_process_of_verification_u2_exp_r3, theater_ratio, 2025, 0.3).

% Extraction over time
narrative_ontology:measurement(epis_be_t1950, epistemic_process_of_verification_u2_exp_r3, base_extractiveness, 1950, 0.25).
narrative_ontology:measurement(epis_be_t1990, epistemic_process_of_verification_u2_exp_r3, base_extractiveness, 1990, 0.4).
narrative_ontology:measurement(epis_be_t2025, epistemic_process_of_verification_u2_exp_r3, base_extractiveness, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u2_exp_r3, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
