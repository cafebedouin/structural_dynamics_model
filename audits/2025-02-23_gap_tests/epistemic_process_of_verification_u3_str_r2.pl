% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u3_str_r2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u3_str_r2, []).

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
 *   constraint_id: epistemic_process_of_verification_u3_str_r2
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   This constraint represents the scientific method's requirement for
 *   independent verification and replication. While essential for building a
 *   reliable body of public knowledge, this process imposes significant costs
 *   on the researchers making novel claims. It functions as a coordination
 *   mechanism for the scientific community but also as an extractive filter
 *   that can stifle or delay paradigm shifts.
 *
 * KEY AGENTS:
 *   - Novel Claim Researchers / Paradigm Challengers: The primary victims, who bear the costs of scrutiny and delay.
 *   - Scientific Establishment (Universities, Journals): The primary beneficiaries and enforcers, who gain stability and authority from the process.
 *   - Funding Agencies: Beneficiaries who use the process to de-risk their investments in research.
 *   - Society at Large: Secondary beneficiaries who rely on the verified outputs of science.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u3_str_r2, 0.55).
domain_priors:suppression_score(epistemic_process_of_verification_u3_str_r2, 0.7).
domain_priors:theater_ratio(epistemic_process_of_verification_u3_str_r2, 0.25).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u3_str_r2, extractiveness, 0.55).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u3_str_r2, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u3_str_r2, theater_ratio, 0.25).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u3_str_r2, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u3_str_r2, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u3_str_r2, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u3_str_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_str_r2, scientific_establishment).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_str_r2, funding_agencies).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_str_r2, journal_publishers).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_str_r2, society_at_large).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u3_str_r2, novel_claim_researchers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u3_str_r2, paradigm_challengers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a researcher with a novel, paradigm-shifting claim, the process feels like a Snare. They are trapped within a system that extracts immense effort, time, and career risk, with their work's acceptance controlled by incumbents who benefit from the status quo. The high suppression of alternative validation paths leaves them no choice but to engage.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_str_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% From the perspective of a funding body, university, or established journal, the process is a pure Rope. It's a coordination mechanism that ensures the stability and reliability of the scientific corpus, filters out noise, and directs resources efficiently. The extraction from individual researchers is seen as a necessary cost for collective epistemic security.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_str_r2, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes the dual nature of the constraint. It performs an essential coordination function (Rope aspect) for the entire scientific enterprise but achieves this via a mechanism that asymmetrically extracts resources and career potential from innovators and challengers (Snare aspect). This makes it a canonical Tangled Rope.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_str_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% For a government or philanthropic entity funding science, the process is a Scaffold supporting public trust. It's a temporary (on a per-claim basis) but necessary structure to ensure that public funds generate reliable, verifiable knowledge. They are constrained in that they cannot easily invent a better system, but powerful enough to defund fields that fail to self-regulate.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_str_r2, scaffold,
    context(agent_power(powerful),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u3_str_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_str_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_str_r2, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u3_str_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u3_str_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55) is high due to the immense cost in time, resources, and career risk imposed on innovators to get their claims validated. Suppression (0.70) is also high because alternative epistemic paths (e.g., claims based on authority or private revelation) are systematically excluded from formal scientific discourse. The theater ratio (0.25) is non-zero, reflecting the 'publish or perish' culture, but the core function remains robust.
 *
 * PERSPECTIVAL GAP:
 *   The significant gap between the innovator's 'Snare' perspective and the establishment's 'Rope' perspective is central to this constraint. One agent experiences it as a coercive trap, while the other experiences it as a vital coordination tool. The analytical 'Tangled Rope' classification reconciles these views by acknowledging both functions are present and structurally linked.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is derived from the flow of costs and benefits. Innovators ('novel_claim_researchers') are designated as victims because they bear the direct costs of the verification process. The scientific establishment, funders, and publishers are beneficiaries as the process solidifies their authority, de-risks their endeavors, and creates the stable knowledge-product they depend on.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a Tangled Rope avoids two errors. It is not a pure Rope, which would ignore the real, asymmetric costs borne by challengers. It is also not a pure Snare, which would deny its indispensable function in coordinating a global community toward a shared understanding of reality. The classification correctly identifies it as a system with both legitimate coordination and significant, asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_as_filter_or_gatekeeping,
    'Is the high extractive cost of verification a necessary filter for quality, or is it primarily a gatekeeping mechanism that preserves existing paradigms and suppresses radical innovation?',
    'Comparative historical analysis of the replication success/failure rate of claims that were initially rejected vs. easily accepted, correlated with their potential to disrupt established theories.',
    'If primarily a quality filter, the constraint is closer to a Rope. If primarily gatekeeping, it is closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_as_filter_or_gatekeeping, empirical, 'Whether the verification process functions more as a necessary quality filter or as a conservative gatekeeping mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u3_str_r2, 1920, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1920, epistemic_process_of_verification_u3_str_r2, theater_ratio, 1920, 0.1).
narrative_ontology:measurement(epis_tr_t1970, epistemic_process_of_verification_u3_str_r2, theater_ratio, 1970, 0.18).
narrative_ontology:measurement(epis_tr_t2020, epistemic_process_of_verification_u3_str_r2, theater_ratio, 2020, 0.25).

% Extraction over time
narrative_ontology:measurement(epis_be_t1920, epistemic_process_of_verification_u3_str_r2, base_extractiveness, 1920, 0.3).
narrative_ontology:measurement(epis_be_t1970, epistemic_process_of_verification_u3_str_r2, base_extractiveness, 1970, 0.45).
narrative_ontology:measurement(epis_be_t2020, epistemic_process_of_verification_u3_str_r2, base_extractiveness, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u3_str_r2, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u3_str_r2, peer_review_process).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u3_str_r2, academic_funding_models).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
