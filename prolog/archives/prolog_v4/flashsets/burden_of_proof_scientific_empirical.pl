% ============================================================================
% CONSTRAINT STORY: burden_of_proof_scientific_empirical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-01-07
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_burden_of_proof_scientific_empirical, []).

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
 *   constraint_id: burden_of_proof_scientific_empirical
 *   human_readable: Statistical Significance Threshold (p < 0.05)
 *   domain: technological/social
 *
 * SUMMARY:
 *   The statistical significance threshold of p < 0.05 serves as a codified
 *   burden of proof in empirical sciences. It aims to filter out spurious
 *   findings but can also suppress novel ideas and disadvantage underfunded
 *   researchers. Established researchers and journals benefit from the
 *   reinforcement of existing paradigms, creating a complex interplay of
 *   coordination and extraction.
 *
 * KEY AGENTS:
 *   - Novel Ideas: Primary victim (powerless/trapped) - are suppressed if unable to meet statistical significance threshold.
 *   - Underfunded Researchers: Secondary victim (moderate/constrained) - face resource barriers to achieving significance.
 *   - Established Researchers: Primary beneficiary (institutional/arbitrage) - benefit from reinforcing existing paradigms.
 *   - Journals: Institutional actor (institutional/constrained) - benefit from seemingly objective criterion for selecting manuscripts, constrained by need to publish significant research.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(burden_of_proof_scientific_empirical, 0.55).
domain_priors:suppression_score(burden_of_proof_scientific_empirical, 0.65).
domain_priors:theater_ratio(burden_of_proof_scientific_empirical, 0.3).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(burden_of_proof_scientific_empirical, extractiveness, 0.55).
narrative_ontology:constraint_metric(burden_of_proof_scientific_empirical, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(burden_of_proof_scientific_empirical, theater_ratio, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(burden_of_proof_scientific_empirical, tangled_rope).
narrative_ontology:human_readable(burden_of_proof_scientific_empirical, "Statistical Significance Threshold (p < 0.05)").
narrative_ontology:topic_domain(burden_of_proof_scientific_empirical, "technological/social").

domain_priors:requires_active_enforcement(burden_of_proof_scientific_empirical).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(burden_of_proof_scientific_empirical, established_researchers).
narrative_ontology:constraint_beneficiary(burden_of_proof_scientific_empirical, journals).
narrative_ontology:constraint_victim(burden_of_proof_scientific_empirical, novel_ideas).
narrative_ontology:constraint_victim(burden_of_proof_scientific_empirical, underfunded_researchers).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% Novel ideas that challenge existing paradigms face an uphill battle to meet the statistical significance threshold, often being suppressed in favor of more easily validated, but less impactful, research.
constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% Researchers with limited resources may struggle to achieve the necessary sample sizes or conduct the rigorous statistical analyses required to meet the p < 0.05 threshold, even if their research has merit. They are constrained by resources but benefit from the standard's existence, which ideally weeds out poor research.
constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, tangled_rope,
    context(agent_power(moderate),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(national))).

% Established researchers benefit from the statistical significance threshold, as it reinforces existing paradigms and makes it more difficult for novel ideas to gain traction.
constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(arbitrage),
            spatial_scope(global))).

% Academic journals benefit from the statistical significance threshold as it provides a seemingly objective criterion for selecting manuscripts, but they are also constrained by the need to publish groundbreaking research.
constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% Significance testing was at one time functionally useful, but is now largely theater, as the ease of collecting, storing, and performing tests on large datasets has incentivized and enabled p-hacking and other forms of statistical malpractice.
constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, piton,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(burden_of_proof_scientific_empirical_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, TypeOther, context(agent_power(moderate), _, _, _)),
    TypePowerless \= TypeOther.

test(extraction_signature) :-
    domain_priors:base_extractiveness(burden_of_proof_scientific_empirical, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

test(piton_threshold) :-
    domain_priors:theater_ratio(burden_of_proof_scientific_empirical, TR),
    TR >= 0.70.

:- end_tests(burden_of_proof_scientific_empirical_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness (0.55): Moderate-high. The statistical significance threshold extracts resources and attention from novel ideas and underfunded researchers, as they must overcome a higher burden of proof. Suppression (0.65): High. The threshold significantly suppresses novel ideas, as they are less likely to be validated easily and may be overshadowed by research conforming to existing paradigms. Theater ratio (0.30): Low. The threshold retains some functional utility, as it still filters out a portion of spurious findings.
 *
 * PERSPECTIVAL GAP:
 *   The perspectival gap arises from the differing positions in the research ecosystem. Novel ideas are snared by the difficulty of achieving statistical significance, while established researchers benefit from its reinforcement of existing paradigms. Underfunded researchers find themselves constrained, while journals benefit from a seemingly objective selection criterion.
 *
 * DIRECTIONALITY LOGIC:
 *   The beneficiaries are those who benefit from reinforcing the status quo, and the victims are those who are disadvantaged by the threshold's bias towards established paradigms. Powerless agents get higher d values and therefore classification as snare.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    alternative_significance_metrics,
    'Are there alternative statistical significance metrics that would be more robust to p-hacking and other forms of statistical malpractice?',
    'Comparative analysis of different statistical significance metrics, including Bayesian methods, effect size measures, and replication rates.',
    'If alternative metrics are found to be more robust, the burden of proof would shift away from the p < 0.05 threshold, potentially reducing the suppression of novel ideas.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_significance_metrics, empirical, 'Alternative significance metrics').

omega_variable(
    role_of_prior_probability,
    'How should prior probabilities be incorporated into statistical significance testing?',
    'Theoretical and empirical analysis of Bayesian methods, including the use of informative priors and sensitivity analyses.',
    'If prior probabilities are found to be important, the burden of proof would shift towards providing strong evidence to overturn well-established prior beliefs.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(role_of_prior_probability, conceptual, 'Role of prior probability').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(burden_of_proof_scientific_empirical, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(burd_tr_t0, burden_of_proof_scientific_empirical, theater_ratio, 0, 0.1).
narrative_ontology:measurement(burd_tr_t5, burden_of_proof_scientific_empirical, theater_ratio, 5, 0.2).
narrative_ontology:measurement(burd_tr_t10, burden_of_proof_scientific_empirical, theater_ratio, 10, 0.3).

% Extraction over time
narrative_ontology:measurement(burd_be_t0, burden_of_proof_scientific_empirical, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(burd_be_t5, burden_of_proof_scientific_empirical, base_extractiveness, 5, 0.45).
narrative_ontology:measurement(burd_be_t10, burden_of_proof_scientific_empirical, base_extractiveness, 10, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(burden_of_proof_scientific_empirical, information_standard).
narrative_ontology:affects_constraint(burden_of_proof_scientific_empirical, replication_crisis).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
