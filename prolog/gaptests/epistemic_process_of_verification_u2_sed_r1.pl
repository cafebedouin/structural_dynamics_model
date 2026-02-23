% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u2_sed_r1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-21
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u2_sed_r1, []).

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
 *   constraint_id: epistemic_process_of_verification_u2_sed_r1
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   The process of scientific verification requires that novel claims be
 *   independently replicated before they are accepted. This serves as a
 *   powerful coordination mechanism, ensuring the reliability of the
 *   scientific record. However, this process is not cost-free. It imposes a
 *   significant burden of time, resources, and career risk on the innovators
 *   making the novel claims, while the benefits of a stable knowledge base
 *   accrue to the entire scientific establishment.
 *
 * KEY AGENTS:
 *   - Innovating Researchers / Junior Scientists: Primary targets (powerless/trapped) — bear the costs of verification.
 *   - Scientific Establishment (Journals, Funding Bodies, Senior Academics): Primary beneficiaries (institutional/arbitrage) — benefit from a stable, reliable knowledge base.
 *   - The Public: Secondary beneficiary — receives more reliable scientific information.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u2_sed_r1, 0.48).
domain_priors:suppression_score(epistemic_process_of_verification_u2_sed_r1, 0.75).
domain_priors:theater_ratio(epistemic_process_of_verification_u2_sed_r1, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_sed_r1, extractiveness, 0.48).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_sed_r1, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_sed_r1, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u2_sed_r1, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u2_sed_r1, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u2_sed_r1, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u2_sed_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_sed_r1, scientific_establishment).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_sed_r1, journal_publishers).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_sed_r1, funding_agencies).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_sed_r1, the_public).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_sed_r1, innovating_researchers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_sed_r1, junior_scientists).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_sed_r1, paradigm_challengers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a junior researcher whose career depends on publishing novel results, the long, arduous, and uncertain process of independent verification feels like a snare that extracts their time and intellectual property for the benefit of a slow-moving establishment.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% From the perspective of an institution like a major journal, the verification process is a pure coordination mechanism (a rope) that ensures the quality and reliability of the scientific record, protecting the journal's reputation and the integrity of the field.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r1, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% A mid-career scientist both benefits from the reliability of the existing literature and pays the cost of getting their own new work verified. They experience the system as a tangled rope, a necessary coordination tool that nonetheless involves significant, asymmetrically-applied costs.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r1, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The analytical observer sees both the essential coordination function that prevents epistemic chaos and the asymmetric extraction of resources (time, funding, career momentum) from innovators to subsidize the stability of the broader scientific commons. This dual function is the definition of a tangled rope.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u2_sed_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r1, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u2_sed_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u2_sed_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.48) reflects the significant cost in time, funding, and career momentum extracted from innovators to produce a public good (reliable knowledge). The suppression score (0.75) is high because there are virtually no legitimate alternative paths to canonizing a scientific claim in established fields. The theater ratio (0.15) is low because the process is highly functional, despite some performative aspects.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the powerless researcher (who sees a Snare that consumes their career for the establishment's benefit) and the institutional editor (who sees a Rope that coordinates the community towards truth). The analytical view of Tangled Rope acknowledges the validity of both: it is a system with a genuine, vital coordination function that is fueled by asymmetric extraction.
 *
 * DIRECTIONALITY LOGIC:
 *   The direction of extraction flows from individual innovators, who must pay the 'verification tax', to the collective scientific establishment, which reaps the benefit of a high-trust information commons. The innovators are the clear victims, while the established institutions and the community at large are the beneficiaries.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a Tangled Rope correctly avoids two errors. It is not a pure Rope, as this would ignore the severe, asymmetrically-borne costs of verification. It is not a pure Snare, as this would deny its absolutely essential function in coordinating scientific activity and filtering error. The Tangled Rope classification captures this essential tension between coordination and extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_as_progress_vs_gatekeeping,
    'Is the high cost of verification a necessary feature for epistemic security, or has it become a form of institutional gatekeeping that primarily serves the established paradigm?',
    'Analysis of replication success rates versus the 'disruptiveness' of the original claim. If highly disruptive (paradigm-challenging) claims have a much lower chance of being verified or published, it suggests gatekeeping.',
    'If it is a necessary feature, the classification leans towards Rope. If it is primarily gatekeeping, it leans towards Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_as_progress_vs_gatekeeping, empirical, 'Whether the verification process is primarily for epistemic security or institutional gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u2_sed_r1, 1920, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1920, epistemic_process_of_verification_u2_sed_r1, theater_ratio, 1920, 0.05).
narrative_ontology:measurement(epis_tr_t1970, epistemic_process_of_verification_u2_sed_r1, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(epis_tr_t2020, epistemic_process_of_verification_u2_sed_r1, theater_ratio, 2020, 0.15).

% Extraction over time
narrative_ontology:measurement(epis_be_t1920, epistemic_process_of_verification_u2_sed_r1, base_extractiveness, 1920, 0.2).
narrative_ontology:measurement(epis_be_t1970, epistemic_process_of_verification_u2_sed_r1, base_extractiveness, 1970, 0.35).
narrative_ontology:measurement(epis_be_t2020, epistemic_process_of_verification_u2_sed_r1, base_extractiveness, 2020, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u2_sed_r1, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
