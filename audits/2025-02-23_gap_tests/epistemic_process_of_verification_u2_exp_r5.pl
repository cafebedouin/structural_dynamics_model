% ============================================================================
% CONSTRAINT STORY: epistemic_process_of_verification_u2_exp_r5
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2023-10-27
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_epistemic_process_of_verification_u2_exp_r5, []).

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
 *   constraint_id: epistemic_process_of_verification_u2_exp_r5
 *   human_readable: Epistemic Process of Scientific Verification
 *   domain: scientific/epistemology
 *
 * SUMMARY:
 *   This constraint represents the scientific method's requirement for
 *   independent replication before a claim is accepted. While it serves an
 *   essential coordination function for the entire scientific
 *   community—building a shared, reliable map of reality—it imposes immense,
 *   asymmetric costs on junior researchers, who perform the bulk of this
 *   high-risk labor. The process filters noise but also extracts career
 *   potential and resources from its most vulnerable participants.
 *
 * KEY AGENTS:
 *   - Junior Researchers/Postdocs: Primary targets (powerless/trapped) who bear the labor and career risk of replication.
 *   - Established Principal Investigators: Primary beneficiaries (institutional/arbitrage) who benefit from the system's stability and quality control.
 *   - Funding Agencies: Institutional enforcers (institutional/constrained) who require verification to justify resource allocation.
 *   - Researchers at underfunded institutions: Secondary victims (moderate/constrained) who are often excluded from participating in cutting-edge verification due to high equipment costs.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(epistemic_process_of_verification_u2_exp_r5, 0.48).
domain_priors:suppression_score(epistemic_process_of_verification_u2_exp_r5, 0.62).
domain_priors:theater_ratio(epistemic_process_of_verification_u2_exp_r5, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r5, extractiveness, 0.48).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r5, suppression_requirement, 0.62).
narrative_ontology:constraint_metric(epistemic_process_of_verification_u2_exp_r5, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(epistemic_process_of_verification_u2_exp_r5, tangled_rope).
narrative_ontology:human_readable(epistemic_process_of_verification_u2_exp_r5, "Epistemic Process of Scientific Verification").
narrative_ontology:topic_domain(epistemic_process_of_verification_u2_exp_r5, "scientific/epistemology").

domain_priors:requires_active_enforcement(epistemic_process_of_verification_u2_exp_r5).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r5, established_principal_investigators).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r5, funding_agencies).
narrative_ontology:constraint_beneficiary(epistemic_process_of_verification_u2_exp_r5, scientific_community_at_large).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r5, junior_researchers_postdocs).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r5, researchers_at_underfunded_institutions).
narrative_ontology:constraint_victim(epistemic_process_of_verification_u2_exp_r5, proposers_of_novel_paradigms).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% For the postdoc or graduate student whose career depends on successfully navigating this process, it feels like a high-stakes snare. The cost of failed replication (in time and reputation) is borne personally, while the benefits of successful replication are diffuse.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r5, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% From the perspective of a tenured lab head, the process is a pure coordination mechanism (Rope) that filters out noise, protects the field's integrity, and ensures that new claims are robust before they overturn established work.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r5, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% The funder sees both sides. They need the coordination to ensure their investments produce reliable knowledge, but they are also aware of the immense waste and human cost (extraction) involved in the process.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r5, tangled_rope,
    context(agent_power(institutional),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% The analytical view recognizes the essential coordination function for building reliable collective knowledge, while also acknowledging the significant and asymmetric costs extracted from the most vulnerable participants in the system.
constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r5, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(epistemic_process_of_verification_u2_exp_r5_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r5, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(epistemic_process_of_verification_u2_exp_r5, TypeOther, context(agent_power(institutional), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(epistemic_process_of_verification_u2_exp_r5, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(epistemic_process_of_verification_u2_exp_r5_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (ε=0.48) reflects the significant cost in time, resources, and career risk imposed on those performing replication. The suppression score (0.62) represents the high bar set by peer review, the cost of entry for building a capable lab, and the institutional pressure to conform to established paradigms. The process is actively enforced by the entire academic apparatus of peer review, grant allocation, and tenure.
 *
 * PERSPECTIVAL GAP:
 *   The gap is stark: for the established PI, the system is a Rope that ensures quality. For the postdoc whose career hinges on replicating a novel, potentially spurious result, it is a Snare. The PI has already passed through the filter and now benefits from its operation, while the postdoc is the one being filtered, bearing the full cost of failure.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries are the established scientific players and the community at large, who receive a reliable knowledge base. The victims are the junior researchers and those at less-prestigious institutions who provide the labor and absorb the risk for this public good. The flow of value is from the labor and risk-taking of the precarious to the stability and certainty of the established.
 *
 * MANDATROPHY ANALYSIS:
 *   Classifying this process as a pure Rope would be a classic mandatrophy error, ignoring the severe extraction from postdocs. Classifying it as a pure Snare would be equally wrong, as it would deny the indispensable coordination function it provides for all of science. The Tangled Rope classification correctly identifies its dual nature: a system of coordination built upon a foundation of asymmetric extraction.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    verification_cost_necessity,
    'Is the high cost imposed on junior researchers an unavoidable feature of epistemic rigor, or a contingent bug of the current academic incentive structure?',
    'Comparative analysis of outcomes from alternative scientific structures, such as registered reports, lottery-based funding, or different models for crediting replication work.',
    'If the cost is a necessary feature, the constraint is closer to a harsh Rope. If it's a contingent bug, it is a classic Tangled Rope that is a prime target for reform.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(verification_cost_necessity, empirical, 'Whether the high extraction from junior scientists is a necessary feature or a reformable bug.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(epistemic_process_of_verification_u2_exp_r5, 1970, 2020).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(epis_tr_t1970, epistemic_process_of_verification_u2_exp_r5, theater_ratio, 1970, 0.1).
narrative_ontology:measurement(epis_tr_t1995, epistemic_process_of_verification_u2_exp_r5, theater_ratio, 1995, 0.22).
narrative_ontology:measurement(epis_tr_t2020, epistemic_process_of_verification_u2_exp_r5, theater_ratio, 2020, 0.3).

% Extraction over time
narrative_ontology:measurement(epis_be_t1970, epistemic_process_of_verification_u2_exp_r5, base_extractiveness, 1970, 0.3).
narrative_ontology:measurement(epis_be_t1995, epistemic_process_of_verification_u2_exp_r5, base_extractiveness, 1995, 0.4).
narrative_ontology:measurement(epis_be_t2020, epistemic_process_of_verification_u2_exp_r5, base_extractiveness, 2020, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(epistemic_process_of_verification_u2_exp_r5, information_standard).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u2_exp_r5, academic_tenure_system).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u2_exp_r5, grant_funding_process).
narrative_ontology:affects_constraint(epistemic_process_of_verification_u2_exp_r5, journal_peer_review).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
