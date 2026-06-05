% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u2_sed_r4
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u2_sed_r4, []).

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
 *   constraint_id: epistemic_process_of_verification_u2_sed_r4
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   The process of scientific verification requires independent replication
 *   of novel claims before they are accepted. This serves a vital
 *   coordination function, creating a shared and reliable body of knowledge.
 *   However, it also functions as a powerful and resource-intensive
 *   gatekeeping mechanism, where established labs and paradigms are
 *   structurally advantaged over newcomers with novel or disruptive findings.
 *
 * KEY AGENTS:
 *   - Novel Claim Proposers: Primary targets who bear the costs and risks of the process (powerless/trapped).
 *   - Established Research Labs: Primary beneficiaries who control the means of verification and benefit from the stability (institutional/arbitrage).
 *   - Funding Agencies & Journals: Institutional enforcers of the verification standard.
 *   - The Scientific Community: A collective beneficiary of reliable knowledge, but composed of individuals who are victims or beneficiaries.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u2_sed_r4, 0.55).
domain_priors:suppression_score(epistemic_process_of_verification_u2_sed_r4, 0.75).
domain_priors:theater_ratio(epistemic_process_of_verification_u2_sed_r4, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_sed_r4, extractiveness, 0.55).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_sed_r4, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_sed_r4, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u2_sed_r4, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u2_sed_r4, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u2_sed_r4, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u2_sed_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_sed_r4, established_research_labs).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_sed_r4, scientific_community_at_large).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_sed_r4, funding_agencies).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_sed_r4, journal_publishers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_sed_r4, novel_claim_proposers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_sed_r4, underfunded_researchers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_sed_r4, interdisciplinary_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a researcher with a groundbreaking but unverified result, the process is a costly, slow, and high-risk gatekeeping mechanism they cannot bypass. Their career is trapped pending verification by others.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% For a well-funded, prestigious lab, the verification process is a quality control mechanism that reinforces standards, stabilizes the field, and allows them to leverage their resources to direct scientific consensus.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r4, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% From a position of moderate security, one can see both the necessity of the coordination function for reliable knowledge and the extractive costs imposed on those without resources or institutional backing.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r4, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(national))).

% The analytical view recognizes the essential coordination function that builds cumulative knowledge, while also seeing the structural extraction and gatekeeping that privileges established paradigms and institutions over novel ones.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r4, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u2_sed_r4_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r4, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_sed_r4, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u2_sed_r4, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u2_sed_r4_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.55) reflects the significant cost in time, funding, and career progression that the verification process demands, a cost borne disproportionately by the claimant. The high suppression (0.75) indicates the lack of any viable alternative path for a claim to be accepted as scientific fact. Active enforcement is carried out through the mechanisms of peer review and funding allocation.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the Novel Claim Proposer, who experiences the process as a Snare (a costly and potentially impassable barrier to their career), and the Established Laboratory, which sees it as a Rope (an orderly and necessary process for quality control). The analytical view reconciles these by identifying the structure as a Tangled Rope, possessing both functions simultaneously.
 *
 * DIRECTIONALITY LOGIC:
 *   The constraint extracts resources (time, funding, opportunity cost) from those making new claims and transfers stability, prestige, and agenda-setting power to the established institutions that control the verification process. The scientific community at large is a beneficiary of the output (reliable knowledge) but the costs of production are not borne symmetrically.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a Tangled Rope correctly identifies its dual nature. A pure Rope classification would ignore the significant, asymmetric extraction and gatekeeping power. A pure Snare classification would fail to recognize the indispensable coordination function it serves in building a consensus reality for science. The Tangled Rope captures the reality that a necessary function is coupled with an extractive social structure.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_cost_origin,
    'Is the high cost and slow pace of verification an irreducible property of discovering complex truths, or a contingent artifact of institutional gatekeeping and risk aversion?',
    'Comparative analysis of verification times and costs in fields with different funding models, institutional structures, or levels of computational automation (e.g., open-source AI vs. traditional condensed matter physics).',
    'If the cost is irreducible, the constraint has Mountain-like properties. If it is a contingent artifact of social structures, it is more purely a Snare for newcomers.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_cost_origin, empirical, 'Whether the high cost of verification is a natural floor or a socially constructed barrier.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u2_sed_r4, 1950, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1950, epistemic_process_of_verification_u2_sed_r4, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(epis_tr_t1985, epistemic_process_of_verification_u2_sed_r4, theater_ratio, 1985, 0.15).
narrative_ontology:measurement(epis_tr_t2024, epistemic_process_of_verification_u2_sed_r4, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(epis_be_t1950, epistemic_process_of_verification_u2_sed_r4, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement(epis_be_t1985, epistemic_process_of_verification_u2_sed_r4, base_extractiveness, 1985, 0.45).
narrative_ontology:measurement(epis_be_t2024, epistemic_process_of_verification_u2_sed_r4, base_extractiveness, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u2_sed_r4, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u2_sed_r4, public_trust_in_science).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u2_sed_r4, pharmaceutical_drug_approval).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
