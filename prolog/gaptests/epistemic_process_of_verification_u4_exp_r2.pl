% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u4_exp_r2
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u4_exp_r2, []).

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
 *   constraint_id: epistemic_process_of_verification_u4_exp_r2
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   The process of scientific verification requires independent replication
 *   of claims before they are accepted as fact. This coordinates the
 *   scientific community towards a shared, reliable understanding of reality
 *   but imposes significant costs on researchers, especially those
 *   challenging existing paradigms or operating with limited resources. It
 *   functions as a powerful gatekeeping mechanism, enforced through peer
 *   review and funding decisions.
 *
 * KEY AGENTS:
 *   - Paradigm Challengers / Early-Career Researchers: Primary targets (powerless/trapped)
 *   - Scientific Establishment (Journals, Universities, Funding Agencies): Primary beneficiaries and enforcers (institutional/arbitrage)
 *   - Tenured Incumbent Researchers: Secondary beneficiaries (powerful/mobile)
 *   - Analytical Observers (Historians/Philosophers of Science): Observers (analytical/analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u4_exp_r2, 0.55).
domain_priors:suppression_score(epistemic_process_of_verification_u4_exp_r2, 0.65).
domain_priors:theater_ratio(epistemic_process_of_verification_u4_exp_r2, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u4_exp_r2, extractiveness, 0.55).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u4_exp_r2, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u4_exp_r2, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u4_exp_r2, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u4_exp_r2, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u4_exp_r2, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u4_exp_r2).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u4_exp_r2, scientific_establishment).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u4_exp_r2, funding_agencies).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u4_exp_r2, journal_publishers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u4_exp_r2, early_career_researchers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u4_exp_r2, underfunded_labs).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u4_exp_r2, paradigm_challengers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For a researcher with a novel but difficult-to-replicate finding, the process is a high-stakes filter that can end a career. The costs are immediate and personal, and there is no alternative path to acceptance within the scientific community.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r2, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% For funding agencies and established journals, the process is a crucial coordination mechanism to ensure quality and prevent the pollution of the scientific record with erroneous claims. The extractive costs are viewed as a necessary price for epistemic stability.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r2, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% An established, well-funded researcher benefits from the stability the system provides and has the resources to navigate its verification demands. While aware of the costs, their position is secured by it, making it function as a coordination system that protects their status.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r2, rope,
    context(agent_power(powerful),
            time_horizon(biographical),
            exit_options(mobile),
            spatial_scope(global))).

% The analytical view recognizes both the indispensable coordination function in creating reliable knowledge and the asymmetric extraction imposed on those with fewer resources or more radical ideas. The process both enables progress and filters it through an institutional bias.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r2, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u4_exp_r2_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r2, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u4_exp_r2, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u4_exp_r2, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u4_exp_r2_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness of 0.55 reflects the high cost in time, funding, and potential career-ending failure for those subjected to the verification process. The suppression score of 0.65 represents the near-impossibility of gaining acceptance for claims that bypass this process, effectively suppressing alternative epistemic routes. The theater ratio is low as the core activity is functional, though it has increased over time with 'publish or perish' pressures.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: for the institutional beneficiary, it is a Rope that ensures quality control and epistemic order. For the powerless researcher with a novel claim, it is a Snare that threatens their career and extracts immense effort with no guarantee of success. This difference arises from their structural position relative to the costs and benefits of the system.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are the institutions that govern science; they gain stability, control, and prestige from the process. The victims are the individual researchers, particularly those who are resource-poor or challenging orthodoxy, who bear the direct costs of replication and the risk of professional failure. The system extracts resources and potential innovations from the latter to subsidize the epistemic security of the former.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a Tangled Rope is crucial. A Rope classification would ignore the severe, asymmetric costs borne by a generation of researchers. A Snare classification would deny its absolutely essential function in creating reliable, cumulative knowledge. The Tangled Rope designation correctly captures this duality: it is a system of coordination that is structurally coupled with a powerful mechanism of extraction and exclusion.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_cost_origin,
    'Is the high cost and slow pace of verification a necessary feature for reliable knowledge acquisition, or an artificially inflated barrier protecting the scientific establishment?',
    'Comparative analysis of scientific progress and career outcomes under alternative, lower-cost verification models (e.g., registered reports, open data platforms).',
    'If the cost is a necessary, irreducible property of epistemology, the constraint is closer to a Rope. If it is an artificially inflated institutional barrier, it is closer to a Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_cost_origin, empirical, 'Whether the high cost of verification is a necessary epistemic feature or an artificial institutional barrier.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u4_exp_r2, 1660, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1660, epistemic_process_of_verification_u4_exp_r2, theater_ratio, 1660, 0.05).
narrative_ontology:measurement(epis_tr_t1950, epistemic_process_of_verification_u4_exp_r2, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(epis_tr_t2024, epistemic_process_of_verification_u4_exp_r2, theater_ratio, 2024, 0.2).

% Extraction over time
narrative_ontology:measurement(epis_be_t1660, epistemic_process_of_verification_u4_exp_r2, base_extractiveness, 1660, 0.25).
narrative_ontology:measurement(epis_be_t1950, epistemic_process_of_verification_u4_exp_r2, base_extractiveness, 1950, 0.4).
narrative_ontology:measurement(epis_be_t2024, epistemic_process_of_verification_u4_exp_r2, base_extractiveness, 2024, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u4_exp_r2, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u4_exp_r2, academic_publishing_model).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u4_exp_r2, university_tenure_system).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
