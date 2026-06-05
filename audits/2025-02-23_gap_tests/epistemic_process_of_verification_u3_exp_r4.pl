% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u3_exp_r4
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u3_exp_r4, []).

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
 *   constraint_id: epistemic_process_of_verification_u3_exp_r4
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   This constraint represents the scientific method's requirement for
 *   independent replication before a claim is accepted. This process
 *   coordinates the scientific community towards a shared, reliable
 *   understanding of reality, but it does so by extracting significant
 *   resources (time, funding, career opportunities) from the individuals and
 *   labs tasked with proposing and verifying novel claims. Its function is
 *   therefore inherently dual: coordination through extraction.
 *
 * KEY AGENTS:
 *   - Early Career Researchers: Primary targets of extraction (powerless/trapped)
 *   - National Funding Agencies: Primary beneficiaries of coordination (institutional/arbitrage)
 *   - Established Principal Investigators: Enforcers and participants who experience both coordination and extraction (powerful/mobile)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u3_exp_r4, 0.48).
domain_priors:suppression_score(epistemic_process_of_verification_u3_exp_r4, 0.62).
domain_priors:theater_ratio(epistemic_process_of_verification_u3_exp_r4, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u3_exp_r4, extractiveness, 0.48).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u3_exp_r4, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u3_exp_r4, theater_ratio, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u3_exp_r4, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u3_exp_r4, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u3_exp_r4, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u3_exp_r4).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_exp_r4, scientific_community_as_a_whole).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_exp_r4, national_funding_agencies).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u3_exp_r4, downstream_technology_sectors).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u3_exp_r4, novel_claim_proposers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u3_exp_r4, replicating_laboratories).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u3_exp_r4, early_career_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a researcher whose career depends on validating a novel claim, the process is a high-stakes gauntlet that can feel purely extractive and coercive.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r4, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% From the perspective of an institution allocating capital, the verification process is a pure coordination mechanism to ensure resources flow to reliable knowledge, minimizing waste.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r4, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% An established lab head recognizes the coordination benefit but is also acutely aware of the resource cost and reputational risk (extraction) of both proposing and replicating findings.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r4, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% The analytical view sees the essential coordination function (building reliable knowledge) and the inherent, asymmetric extraction (costs borne by individuals) as inseparable features of the same system.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r4, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u3_exp_r4_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r4, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u3_exp_r4, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u3_exp_r4, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u3_exp_r4_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.48) reflects the immense cost in labor, equipment, and career risk required to validate a significant scientific claim. The suppression score (0.62) represents the high bar for acceptance, which actively filters out unverified or erroneous claims, a core feature of the system. The process is actively enforced through peer review and funding allocation, justifying the Tangled Rope classification.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark. For a funding agency, the process is an efficient Rope for managing its portfolio of knowledge-capital. For an early-career researcher, it is a Snare where a single failure to replicate, regardless of cause, can be career-ending. The analytical view of Tangled Rope reconciles these by acknowledging that the Rope's coordination function is paid for by the Snare-like experience of its participants.
 *
 * DIRECTIONALITY LOGIC:
 *   The system's directionality is from the individual to the collective. It extracts resources and career-risk from individual researchers (the 'victims') and transforms it into epistemic certainty for the benefit of the entire scientific field and its institutional funders (the 'beneficiaries').
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a pure Snare would be a mandatrophy error, as it ignores the irreplaceable coordination function of building consensus reality. Conversely, classifying it as a pure Rope ignores the brutal, asymmetric costs borne by junior scientists. The Tangled Rope classification is essential to see both functions simultaneously, allowing for analysis of how to make the process less extractive without compromising its coordinating purpose.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_efficiency_vs_gatekeeping,
    'Is the high cost of verification an irreducible property of discovering truth, or is it inflated by social gatekeeping and risk aversion within the scientific establishment?',
    'Comparative analysis of verification costs and timelines across different scientific fields and historical periods, correlated with funding models.',
    'If the cost is irreducible, the constraint leans towards a Mountain of epistemology. If it is socially inflated, it is a classic Tangled Rope that could be reformed to be less extractive.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_efficiency_vs_gatekeeping, empirical, 'Distinguishing between the necessary epistemic cost and socially constructed gatekeeping costs in scientific verification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u3_exp_r4, 1975, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1975, epistemic_process_of_verification_u3_exp_r4, theater_ratio, 1975, 0.05).
narrative_ontology:measurement(epis_tr_t2000, epistemic_process_of_verification_u3_exp_r4, theater_ratio, 2000, 0.1).
narrative_ontology:measurement(epis_tr_t2025, epistemic_process_of_verification_u3_exp_r4, theater_ratio, 2025, 0.15).

% Extraction over time
narrative_ontology:measurement(epis_be_t1975, epistemic_process_of_verification_u3_exp_r4, base_extractiveness, 1975, 0.3).
narrative_ontology:measurement(epis_be_t2000, epistemic_process_of_verification_u3_exp_r4, base_extractiveness, 2000, 0.41).
narrative_ontology:measurement(epis_be_t2025, epistemic_process_of_verification_u3_exp_r4, base_extractiveness, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u3_exp_r4, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u3_exp_r4, public_trust_in_science).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u3_exp_r4, pharmaceutical_drug_approval_process).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
