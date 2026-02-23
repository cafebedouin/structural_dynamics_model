% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u1_exp_r1
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u1_exp_r1, []).

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
 *   constraint_id: epistemic_process_of_verification_u1_exp_r1
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   This constraint represents the scientific method's requirement for
 *   independent verification and replication before a claim is accepted into
 *   the canon. It functions as a powerful coordination mechanism for the
 *   entire scientific community, ensuring a reliable and robust shared
 *   understanding of reality. However, this process is not without cost,
 *   imposing significant burdens on the individuals and labs who must perform
 *   the original and replicative work.
 *
 * KEY AGENTS:
 *   - Junior Researchers (postdocs, grad students): Primary victims, providing labor for replication.
 *   - Novel Claimants (scientists with new findings): Bear the initial burden of proof and reputational risk.
 *   - Scientific Institutions (universities, funding agencies): Primary beneficiaries and enforcers of the standard.
 *   - The Public / Scientific Community: Collective beneficiaries of a reliable knowledge base.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u1_exp_r1, 0.55).
domain_priors:suppression_score(epistemic_process_of_verification_u1_exp_r1, 0.75).
domain_priors:theater_ratio(epistemic_process_of_verification_u1_exp_r1, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u1_exp_r1, extractiveness, 0.55).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u1_exp_r1, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u1_exp_r1, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u1_exp_r1, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u1_exp_r1, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u1_exp_r1, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u1_exp_r1).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u1_exp_r1, scientific_community_as_a_whole).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u1_exp_r1, funding_agencies).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u1_exp_r1, the_public).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u1_exp_r1, novel_claimants).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u1_exp_r1, junior_researchers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u1_exp_r1, proponents_of_unconventional_theories).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: For a postdoc or graduate student tasked with laborious, low-prestige replication work, the system extracts their labor for the benefit of the commons with little direct career upside and high opportunity cost. Exit is difficult without abandoning their chosen career.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r1, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: For an institution managing a portfolio of research, the verification process is a pure coordination mechanism to ensure the quality and reliability of its investments, de-risking the knowledge base it is building. The extraction is a necessary cost of quality control.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r1, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(national))).

% PERSPECTIVE 3: For a tenured professor with a groundbreaking but controversial claim, the process is both a path to canonization (coordination) and a grueling, resource-intensive ordeal that puts their reputation on the line (extraction).
constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r1, tangled_rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 4: The analytical view recognizes both the indispensable coordination function of building a reliable, shared map of reality and the inherent, asymmetric extraction of resources from individuals to power that process.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r1, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u1_exp_r1_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r1, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r1, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u1_exp_r1, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u1_exp_r1_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.55) reflects the significant cost in time, funding, and career progression extracted from researchers to validate claims. The suppression score (0.75) is high because the process actively and effectively prevents uncorroborated claims from being accepted as scientific fact, enforcing a specific epistemology. The theater ratio is low as the core activity is functional, though it has increased with the pressure to publish in high-impact journals.
 *
 * PERSPECTIVAL GAP:
 *   A significant gap exists between the institutional perspective (Rope), which sees a necessary quality-control mechanism, and the junior researcher's perspective (Snare), which experiences an extractive labor system with high personal risk and deferred, uncertain rewards. The analytical view (Tangled Rope) reconciles these by acknowledging the system has both an essential coordination function and a structurally extractive nature.
 *
 * DIRECTIONALITY LOGIC:
 *   The direction of extraction flows from individual researchers (both claimants and replicators) to the collective institution of science. Beneficiaries are those who rely on the resulting stable knowledge base (the entire community, funding bodies, the public), while victims are those who must pay the direct cost in labor and risk to produce that stability.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a Tangled Rope prevents two errors. It avoids mislabeling it as a pure Rope, which would ignore the significant, asymmetric costs borne by junior scientists and innovators. It also avoids mislabeling it as a pure Snare, which would deny its profoundly effective and necessary function in coordinating a global community towards reliable knowledge.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    labor_model_necessity,
    'Is the high career cost for junior researchers performing replication a necessary feature for scientific rigor or an exploitative, path-dependent labor model?',
    'Comparative analysis of scientific output quality vs. career outcomes in fields with different replication/crediting models (e.g., registered reports).',
    'If found to be a purely exploitative labor model, the constraint's base extractiveness would be higher, pushing it closer to a Snare from the analytical perspective. If necessary, it reinforces the Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(labor_model_necessity, empirical, 'Whether the career cost of replication is necessary for rigor or an exploitative labor model.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u1_exp_r1, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1970, epistemic_process_of_verification_u1_exp_r1, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(epis_tr_t1995, epistemic_process_of_verification_u1_exp_r1, theater_ratio, 1995, 0.15).
narrative_ontology:measurement(epis_tr_t2020, epistemic_process_of_verification_u1_exp_r1, theater_ratio, 2020, 0.2).

% Extraction over time
narrative_ontology:measurement(epis_be_t1970, epistemic_process_of_verification_u1_exp_r1, base_extractiveness, 1970, 0.4).
narrative_ontology:measurement(epis_be_t1995, epistemic_process_of_verification_u1_exp_r1, base_extractiveness, 1995, 0.48).
narrative_ontology:measurement(epis_be_t2020, epistemic_process_of_verification_u1_exp_r1, base_extractiveness, 2020, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u1_exp_r1, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
