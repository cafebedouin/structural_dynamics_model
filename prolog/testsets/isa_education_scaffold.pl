% ============================================================================
% CONSTRAINT STORY: isa_education_scaffold
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-15
% ============================================================================

:- module(constraint_isa_education_scaffold, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

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
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: isa_education_scaffold
 * human_readable: Income Share Agreement (ISA) Funding for Education
 * domain: economic/educational
 * * SUMMARY:
 * An Income Share Agreement (ISA) allows students to access education with
 * zero upfront cost in exchange for a fixed percentage of future earnings
 * over a set term. It acts as a Scaffold (temporary support) that enables
 * upward mobility but creates a localized Snare (income extraction) during
 * the repayment window. Its classification depends heavily on the presence
 * and robustness of its sunset clause.
 * * KEY AGENTS:
 * - The Repaying Graduate: Subject (Powerless). Capital-constrained, feels the extraction directly.
 * - The ISA Provider/Funder: Beneficiary (Institutional). Underwrites the risk for a share of the upside.
 * - The Policy Analyst: Auditor (Analytical). Evaluates the structure's net social benefit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(isa_education_scaffold, 0.42). % Below Snare threshold but high enough to be felt.
domain_priors:suppression_score(isa_education_scaffold, 0.45).   % Moderate: Traditional loans or self-funding are alternatives.
domain_priors:theater_ratio(isa_education_scaffold, 0.20).       % Low: The contract is functional and direct.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(isa_education_scaffold, extractiveness, 0.42).
narrative_ontology:constraint_metric(isa_education_scaffold, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(isa_education_scaffold, theater_ratio, 0.20).

% Constraint self-claim (what does the constraint claim to be?)
narrative_ontology:constraint_claim(isa_education_scaffold, scaffold).

% Binary flags
narrative_ontology:has_sunset_clause(isa_education_scaffold).      % Mandatory for Scaffold classification.
domain_priors:requires_active_enforcement(isa_education_scaffold). % Required for income collection.

% Structural property derivation hooks:
narrative_ontology:constraint_beneficiary(isa_education_scaffold, isa_provider).
narrative_ontology:constraint_victim(isa_education_scaffold, repaying_graduate).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   ========================================================================== */

% PERSPECTIVE 1: THE REPAYING GRADUATE (SNARE)
% During the repayment window, the graduate feels the income share as a Snare.
% Effective extraction χ = 0.42 * π(powerless:1.5) * σ(national:1.0) = 0.63.
% This high perceived extraction limits biographical mobility and savings.
constraint_indexing:constraint_classification(isa_education_scaffold, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE ISA PROVIDER (ROPE)
% The funder sees the ISA as a Rope, coordinating capital with talent and
% diversifying risk across a cohort.
% Effective extraction χ = 0.42 * π(institutional:-0.2) * σ(global:1.2) = -0.10.
% The negative value indicates it's seen as a value-generating investment.
constraint_indexing:constraint_classification(isa_education_scaffold, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE POLICY ANALYST (SCAFFOLD)
% Analytically, the constraint is a Scaffold. Its primary function is temporary
% support, and its existence is contingent on the sunset clause which prevents
% it from becoming a permanent Snare.
constraint_indexing:constraint_classification(isa_education_scaffold, scaffold,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(isa_education_scaffold_tests).

test(perspectival_gap_snare_vs_rope) :-
    % Verify the core perspectival gap between the subject and beneficiary.
    constraint_indexing:constraint_classification(isa_education_scaffold, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(isa_education_scaffold, rope, context(agent_power(institutional), _, _, _)).

test(scaffold_structural_validation) :-
    % A constraint can only be a Scaffold if a sunset clause is declared.
    (   narrative_ontology:has_sunset_clause(isa_education_scaffold)
    ->  constraint_indexing:constraint_classification(isa_education_scaffold, scaffold, _)
    ;   \+ constraint_indexing:constraint_classification(isa_education_scaffold, scaffold, _)
    ).

test(beneficiary_and_victim_declared) :-
    % Scaffolds and high-extraction constraints require beneficiaries and victims.
    narrative_ontology:constraint_beneficiary(isa_education_scaffold, _),
    narrative_ontology:constraint_victim(isa_education_scaffold, _).

:- end_tests(isa_education_scaffold_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The ISA is a canonical example of a constraint whose classification is
 * dominated by its temporal features. The base extractiveness (0.42) is
 * moderate, but when amplified by the powerless index (χ=0.63), it is
 * perceived as a Snare by the graduate during their repayment years.
 * Conversely, the institutional funder perceives it as a pure Rope (χ=-0.10),
 * a mechanism for productive coordination.
 *
 * The analytical classification resolves this tension by identifying the
 * constraint's temporary nature. The `has_sunset_clause/1` fact is critical;
 * without it, the system would classify as a Tangled Rope or Snare from the
 * analytical perspective. The ISA is therefore a Scaffold: a temporary,
 * high-support structure with a defined end.
 *
 * MANDATROPHY ANALYSIS:
 * This structure avoids Mandatrophy by tying extraction directly to the
 * value it creates (income). However, a risk remains: if the education
 * provided ceases to generate a sufficient income premium, the coordination
 * function fails, but the extraction mechanism remains. This would degrade
 * the Scaffold into a pure Snare. The sunset clause provides a hard backstop
 * against this failure mode.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_isa_mobility_threshold,
    'At what level of income does the specified percentage extraction become a net-negative for social mobility, negating the benefit of the education?',
    'Longitudinal study comparing lifetime earnings and wealth accumulation of ISA-funded graduates vs. traditionally-funded peers in high-earning fields.',
    'If extraction outweighs mobility gain, the Scaffold is a mis-classified Snare.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(isa_education_scaffold, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Although extraction is below the 0.46 threshold, temporal data is included
% to model the lifecycle of this borderline constraint. Here we model stable
% extraction terms but a slight increase in marketing/administrative overhead
% (theater_ratio).

% Theater ratio over time (triggers metric_substitution detection):
narrative_ontology:measurement(isa_tr_t0, isa_education_scaffold, theater_ratio, 0, 0.15).
narrative_ontology:measurement(isa_tr_t5, isa_education_scaffold, theater_ratio, 5, 0.18).
narrative_ontology:measurement(isa_tr_t10, isa_education_scaffold, theater_ratio, 10, 0.20).

% Extraction over time (triggers extraction_accumulation detection):
narrative_ontology:measurement(isa_ex_t0, isa_education_scaffold, base_extractiveness, 0, 0.42).
narrative_ontology:measurement(isa_ex_t5, isa_education_scaffold, base_extractiveness, 5, 0.42).
narrative_ontology:measurement(isa_ex_t10, isa_education_scaffold, base_extractiveness, 10, 0.42).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
narrative_ontology:coordination_type(isa_education_scaffold, resource_allocation).

% Network relationships (structural influence edges)
% ISAs are an alternative to, and thus influence, the broader system of student debt.
narrative_ontology:affects_constraint(isa_education_scaffold, student_debt_burden).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */