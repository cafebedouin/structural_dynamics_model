% ============================================================================
% CONSTRAINT STORY: burden_of_proof_scientific_empirical
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-15
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
 *   The convention of using a p-value threshold of < 0.05 to claim
 *   statistical significance is a core coordination mechanism in the
 *   empirical sciences. Initially proposed as a heuristic, it has ossified
 *   into a rigid gatekeeper for publication and funding. This creates a
 *   powerful incentive structure that rewards 'positive' results and
 *   suppresses 'null' findings, leading to systemic problems like publication
 *   bias (the 'file drawer problem') and the replication crisis. The
 *   constraint's structure is not monolithic; it functions simultaneously as
 *   a useful standard, a career-destroying trap, and a performative ritual,
 *   depending on the agent's structural position.
 *
 * KEY AGENTS:
 *   - Researchers with 'significant' results: Primary beneficiaries (institutional/arbitrage) who see a useful coordination standard (Rope).
 *   - Researchers with null results: Primary victims (powerless/trapped) who face a career-limiting extractive system (Snare).
 *   - Journal editors & reviewers: Institutional enforcers (institutional/constrained) who maintain a degraded, inertial standard (Piton).
 *   - Open Science reformers: Organized challengers (organized/mobile) who see a temporary system to be replaced (Scaffold).
 *   - Field epistemic reliability: Abstract victim (powerless/trapped) damaged by the distorted scientific record.
 *   - Analytical meta-scientists: Observers (analytical/analytical) who see the dual coordination/extraction function (Tangled Rope).
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(burden_of_proof_scientific_empirical, 0.65).
domain_priors:suppression_score(burden_of_proof_scientific_empirical, 0.75).
domain_priors:theater_ratio(burden_of_proof_scientific_empirical, 0.6).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(burden_of_proof_scientific_empirical, extractiveness, 0.65).
narrative_ontology:constraint_metric(burden_of_proof_scientific_empirical, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(burden_of_proof_scientific_empirical, theater_ratio, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(burden_of_proof_scientific_empirical, tangled_rope).
narrative_ontology:human_readable(burden_of_proof_scientific_empirical, "Statistical Significance Threshold (p < 0.05)").
narrative_ontology:topic_domain(burden_of_proof_scientific_empirical, "technological/social").

domain_priors:requires_active_enforcement(burden_of_proof_scientific_empirical).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(burden_of_proof_scientific_empirical, researchers_with_significant_results).
narrative_ontology:constraint_beneficiary(burden_of_proof_scientific_empirical, journal_publishers).
narrative_ontology:constraint_victim(burden_of_proof_scientific_empirical, researchers_with_null_results).
narrative_ontology:constraint_victim(burden_of_proof_scientific_empirical, field_epistemic_reliability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: RESEARCHER WITH NULL RESULTS (SNARE) — Trapped by a system that devalues and suppresses their work, extracting career potential and funding. The high suppression (0.75) and high base extraction (0.65) lead to a high effective extraction (χ > 0.66) from this viewpoint.
constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: RESEARCHER WITH 'SIGNIFICANT' RESULTS (ROPE) — Experiences the threshold as a pure coordination mechanism that validates their work and enables publication. As a primary beneficiary with arbitrage (can choose which high-impact journal to publish in), their effective extraction is negative (χ < 0).
constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, rope,
    context(agent_power(institutional),
            time_horizon(immediate),
            exit_options(arbitrage),
            spatial_scope(global))).

% PERSPECTIVE 3: ANALYTICAL OBSERVER (TANGLED ROPE) — Sees both the genuine coordination function (a common standard) and the severe asymmetric extraction (publication bias, file-drawer problem). The constraint requires active enforcement by reviewers and editors, fulfilling the three requirements for a Tangled Rope.
constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% PERSPECTIVE 4: JOURNAL EDITOR (PITON) — Enforces a standard known to be flawed and arbitrary (p=0.049 vs p=0.051). The function has degraded, but the ritual persists due to institutional inertia and lack of a consensus alternative. The theater_ratio (0.60) is significant, though below the hard 0.70 gate; from this actor's perspective of ritual enforcement, the performative aspect is dominant.
constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, piton,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(constrained),
            spatial_scope(global))).

% PERSPECTIVE 5: OPEN SCIENCE REFORMER (SCAFFOLD) — Views the p<0.05 rule as a temporary, flawed system to be replaced. They are building the alternative (pre-registration, Bayesian methods, focus on effect sizes) which acts as a sunset clause on the old standard's dominance. They are organized and have mobility to create new publication venues and norms.
constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, scaffold,
    context(agent_power(organized),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 6: THE NAIVE EMPIRICIST (MOUNTAIN) — Misinterprets a social convention as a fundamental law of inference. Believes that *some* arbitrary cutoff is an unavoidable, natural feature of empirical science. The engine will flag this as a false summit, as the high ε and suppression values are inconsistent with a natural law.
constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, mountain,
    context(agent_power(powerful),
            time_horizon(civilizational),
            exit_options(constrained),
            spatial_scope(universal))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(burden_of_proof_scientific_empirical_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(burden_of_proof_scientific_empirical, TypeOther, context(agent_power(institutional), _, _, _)),
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
 *   Extractiveness (0.65) is high due to the severe career and funding consequences of failing to meet the threshold. Suppression (0.75) is very high, as publishing null results in many fields is exceptionally difficult, effectively silencing a large portion of scientific work. Theater Ratio (0.60) is significant, reflecting the arbitrary nature of the 0.05 cutoff and the ritualistic 'p-hacking' behaviors it encourages to achieve significance, often divorced from genuine scientific inquiry.
 *
 * PERSPECTIVAL GAP:
 *   The profound gap between the beneficiary's Rope and the victim's Snare is the core tension. One agent's useful convention is another's coercive trap. The analytical Tangled Rope classification correctly identifies that both are true simultaneously: the constraint has a genuine coordination function *and* a severe, asymmetric extractive function. The Piton and Scaffold perspectives highlight the constraint's lifecycle dynamics: it is both degrading through inertia and being actively challenged by reformers.
 *
 * DIRECTIONALITY LOGIC:
 *   Beneficiaries (researchers with positive results) have arbitrage exit options (they can shop their paper to multiple journals), leading to a low 'd' value and a Rope classification. Victims (researchers with null results) are trapped, with few venues for their work, leading to a high 'd' value and a Snare classification. Institutional actors like editors are constrained by norms, while organized reformers are mobile enough to create new norms. This differentiation in exit options and structural benefit drives the diverse classifications from a single set of base metrics.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is a classic example of mandatrophy resolution. A naive analysis might label the p-value system as simply 'bad' (Snare) or 'a necessary convention' (Rope). Deferential Realism avoids this by showing that these are perspectival truths. The complete description is the full set of classifications indexed by structural position. The system is a Tangled Rope from the analytical view precisely because it generates these valid, yet contradictory, experiences for agents within it.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    file_drawer_causality,
    'To what extent is the ''file drawer problem'' (suppression of null results) caused by the p<0.05 threshold versus an inherent human bias towards positive or novel findings?',
    'Analysis of publication rates from pre-registered report repositories, which commit to publishing regardless of the outcome.',
    'If the problem is primarily caused by the threshold, the constraint is a strong Snare. If caused by inherent bias, the constraint is more of a Piton that formalizes a pre-existing tendency.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(file_drawer_causality, empirical, 'Causality of the file drawer problem: p-value threshold vs. inherent bias.').

omega_variable(
    alternative_framework_viability,
    'Are proposed alternatives (e.g., Bayesian factors, confidence intervals) fundamentally less gameable, or do they simply introduce new, more subtle forms of extraction and theater?',
    'Longitudinal study of scientific sub-fields that adopt alternative standards, measuring changes in replicability and researcher behavior.',
    'If alternatives are robustly better, the Scaffold perspective is confirmed. If they are equally gameable, the constraint may be a persistent Piton or even a Mountain of social dynamics.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_framework_viability, empirical, 'Gameability and viability of alternative statistical frameworks.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(burden_of_proof_scientific_empirical, 1950, 2015).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(burd_tr_t0, burden_of_proof_scientific_empirical, theater_ratio, 0, 0.2).
narrative_ontology:measurement(burd_tr_t30, burden_of_proof_scientific_empirical, theater_ratio, 30, 0.45).
narrative_ontology:measurement(burd_tr_t65, burden_of_proof_scientific_empirical, theater_ratio, 65, 0.6).

% Extraction over time
narrative_ontology:measurement(burd_be_t0, burden_of_proof_scientific_empirical, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(burd_be_t30, burden_of_proof_scientific_empirical, base_extractiveness, 30, 0.5).
narrative_ontology:measurement(burd_be_t65, burden_of_proof_scientific_empirical, base_extractiveness, 65, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(burden_of_proof_scientific_empirical, information_standard).
narrative_ontology:affects_constraint(burden_of_proof_scientific_empirical, replication_crisis_social_science).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
