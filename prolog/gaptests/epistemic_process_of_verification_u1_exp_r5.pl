% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u1_exp_r5
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-05-15
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u1_exp_r5, []).

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
 *   constraint_id: epistemic_process_of_verification_u1_exp_r5
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   This constraint represents the scientific method's requirement for
 *   independent replication and corroboration before a novel claim is
 *   accepted. While essential for coordinating the scientific community
 *   towards a shared, reliable understanding of reality, this process imposes
 *   significant costs and risks, particularly on those proposing disruptive
 *   ideas. It functions as both a quality filter and a potential gatekeeping
 *   mechanism.
 *
 * KEY AGENTS:
 *   - Novel Claim Proposers: Primary targets of the verification burden (organized/constrained).
 *   - Established Paradigm Holders: Primary beneficiaries and enforcers of the standard (institutional/arbitrage).
 *   - Early Career Researchers: Provide much of the labor for replication (powerless/trapped).
 *   - Scientific Community at Large: Secondary beneficiary of a reliable knowledge base.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u1_exp_r5, 0.48).
domain_priors:suppression_score(epistemic_process_of_verification_u1_exp_r5, 0.62).
domain_priors:theater_ratio(epistemic_process_of_verification_u1_exp_r5, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u1_exp_r5, extractiveness, 0.48).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u1_exp_r5, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u1_exp_r5, theater_ratio, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u1_exp_r5, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u1_exp_r5, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u1_exp_r5, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u1_exp_r5).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u1_exp_r5, scientific_community_at_large).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u1_exp_r5, established_paradigm_holders).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u1_exp_r5, funding_agencies).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u1_exp_r5, novel_claim_proposers).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u1_exp_r5, early_career_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% From the perspective of a research group proposing a disruptive finding, the process is a high-stakes gauntlet. The burden of proof is immense, the career risk from failed replication is severe, and the process can be weaponized by incumbents to stifle innovation. The lack of viable alternatives to this process makes it feel coercive.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r5, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(global))).

% For established institutions, journals, and senior figures, the verification process is a pure coordination mechanism. It maintains the integrity and stability of the field, filters out noise, and reinforces the existing paradigm. The costs are largely externalized to claimants and junior researchers, making it appear as a low-extraction, high-value public good.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r5, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The analytical view recognizes both the indispensable coordination function (creating a shared, reliable map of reality) and the significant, asymmetrically-applied extraction (career risk, resource cost, potential for incumbent gatekeeping). It is a classic Tangled Rope, where a vital collective good is maintained via a coercive and extractive process.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r5, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% For graduate students or postdocs tasked with laborious replication studies, the process is a Snare. Their labor is extracted to validate or invalidate the work of others, often for little personal career advancement, under the coercive pressure of institutional degree or employment requirements.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r5, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(local))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u1_exp_r5_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r5, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u1_exp_r5, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u1_exp_r5, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u1_exp_r5_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness of 0.48 reflects the immense cost in resources, time, and career risk imposed on claimants. The suppression score of 0.62 reflects the fact that there is no viable alternative to this process within mainstream science; bypassing it results in being labeled as pseudoscientific. The theater ratio is low but non-zero, acknowledging the existence of 'replication theater' while affirming the process's core functionality.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: for institutional incumbents who benefit from stability, it is a pure Rope for coordination. For innovators who bear the risk and cost, it is a Snare that can end careers and suppress new ideas. The analytical view must hold both truths at once, classifying it as a Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   Directionality is driven by the flow of risk and resources. Novel Claim Proposers and Early Career Researchers are the primary victims; they invest their labor, resources, and reputation. The beneficiaries are the Established Paradigm Holders, who gain stability and control, and the broader scientific community, which receives a vetted body of knowledge. The system extracts risk-bearing innovation from the periphery to create stability at the core.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a pure Rope would be a form of mandatrophy, ignoring the coercive extraction and career-destroying potential it holds. Conversely, classifying it as a pure Snare would ignore its absolutely essential, world-building function of coordinating to produce reliable knowledge. The Tangled Rope classification is necessary to capture this duality, acknowledging that a vital societal good is produced through a mechanism with significant coercive and extractive properties.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    gatekeeping_vs_quality_control,
    'Is the high burden of verification a necessary feature for ensuring scientific quality, or is it primarily a tool for incumbent paradigms to suppress disruptive innovation?',
    'Analysis of replication success rates correlated with the 'disruptiveness' of the original claim and the institutional status of the claimants vs. replicators.',
    'If primarily for quality control, the constraint leans towards Rope. If primarily for gatekeeping, it leans towards Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(gatekeeping_vs_quality_control, empirical, 'Distinguishing whether the verification burden is for quality control or incumbent gatekeeping.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u1_exp_r5, 1950, 2025).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1950, epistemic_process_of_verification_u1_exp_r5, theater_ratio, 1950, 0.1).
narrative_ontology:measurement(epis_tr_t1987, epistemic_process_of_verification_u1_exp_r5, theater_ratio, 1987, 0.15).
narrative_ontology:measurement(epis_tr_t2025, epistemic_process_of_verification_u1_exp_r5, theater_ratio, 2025, 0.2).

% Extraction over time
narrative_ontology:measurement(epis_be_t1950, epistemic_process_of_verification_u1_exp_r5, base_extractiveness, 1950, 0.3).
narrative_ontology:measurement(epis_be_t1987, epistemic_process_of_verification_u1_exp_r5, base_extractiveness, 1987, 0.4).
narrative_ontology:measurement(epis_be_t2025, epistemic_process_of_verification_u1_exp_r5, base_extractiveness, 2025, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u1_exp_r5, information_standard).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
