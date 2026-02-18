% ============================================================================
% CONSTRAINT STORY: consumer_debt_slavery
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-22
% ============================================================================

:- module(constraint_consumer_debt_slavery, []).

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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: consumer_debt_slavery
 * human_readable: The Interest Trap (Act Your Wage)
 * domain: economic
 * * SUMMARY:
 * A financial constraint where individuals with low capital utilize credit for 
 * depreciating assets or essential consumption, leading to a state of long-term 
 * financial servitude. The extraction occurs through compounding interest payments 
 * that consume future income, preventing wealth accumulation.
 * * KEY AGENTS:
 * - The Borrower: Subject (Powerless) - Lives paycheck-to-paycheck, reliant on credit.
 * - The Creditor: Beneficiary (Institutional) - Profits from interest payments.
 * - The Financial System Auditor: Auditor (Analytical) - Analyzes the structural effects of consumer credit.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(consumer_debt_slavery, 0.72). % Snare extraction >= 0.46
domain_priors:suppression_score(consumer_debt_slavery, 0.60).   % Structural property (raw, unscaled).
domain_priors:theater_ratio(consumer_debt_slavery, 0.45).       % Not a Piton (<= 0.70)

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(consumer_debt_slavery, extractiveness, 0.72).
narrative_ontology:constraint_metric(consumer_debt_slavery, suppression_requirement, 0.60).
narrative_ontology:constraint_metric(consumer_debt_slavery, theater_ratio, 0.45).

% Constraint self-claim (what does the constraint claim to be?)
% The system is framed as a necessary tool for economic liquidity and growth.
narrative_ontology:constraint_claim(consumer_debt_slavery, tangled_rope).
narrative_ontology:human_readable(consumer_debt_slavery, "The Interest Trap (Act Your Wage)").
narrative_ontology:topic_domain(consumer_debt_slavery, "economic").

% Binary flags
domain_priors:requires_active_enforcement(consumer_debt_slavery). % Required for Tangled Rope (e.g., credit reporting, collections)

% Structural property derivation hooks for Tangled Rope:
%   has_coordination_function/1 is DERIVED from constraint_beneficiary/2
%   has_asymmetric_extraction/1 is DERIVED from constraint_victim/2
narrative_ontology:constraint_beneficiary(consumer_debt_slavery, creditor_class).
narrative_ontology:constraint_victim(consumer_debt_slavery, borrower_class).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% High base extraction (0.72) is amplified by powerlessness (π=1.5),
% resulting in an inescapable trap. χ = 0.72 * 1.5 * 1.0 = 1.08.
constraint_indexing:constraint_classification(consumer_debt_slavery, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Institutional power (π=-0.2) inverts extraction, making the system appear
% purely beneficial. χ = 0.72 * -0.2 * 1.2 = -0.17.
constraint_indexing:constraint_classification(consumer_debt_slavery, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid nature: a genuine coordination function (liquidity)
% coupled with severe asymmetric extraction.
constraint_indexing:constraint_classification(consumer_debt_slavery, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(consumer_debt_slavery_tests).

test(perspectival_gap) :-
    % Verify the Borrower sees a Snare while the Institutional Creditor sees a Rope.
    constraint_indexing:constraint_classification(consumer_debt_slavery, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consumer_debt_slavery, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(consumer_debt_slavery, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_properties) :-
    % Verify all three conditions for Tangled Rope are met.
    narrative_ontology:constraint_beneficiary(consumer_debt_slavery, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(consumer_debt_slavery, _),     % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(consumer_debt_slavery).

test(extraction_intensity) :-
    domain_priors:base_extractiveness(consumer_debt_slavery, E),
    E > 0.46.

:- end_tests(consumer_debt_slavery_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The base extraction (0.72) is high because consumer debt on depreciating
 * assets or for basic consumption provides zero financial return for the
 * borrower while generating guaranteed, compounding income for the creditor.
 * The suppression score (0.60) reflects the lack of viable alternatives for
 * low-income individuals to access capital or manage financial emergencies.
 *
 * The perspectival gap is stark: for the powerless, it's a Snare that consumes
 * their future. For the institutional beneficiary, it's a Rope that provides
 * essential market liquidity and stable returns. The Tangled Rope classification
 * from the analytical view is critical, as it acknowledges the system's dual
 * nature, preventing a simplistic "pure evil" (Snare) or "pure good" (Rope)
 * assessment.
 *
 * * [RESOLVED MANDATROPHY]:
 * The narrative claim of "modern-day slavery" is resolved by the Tangled Rope
 * classification. It correctly identifies that while the system coordinates
 * transactions (a Rope-like function), the severe, asymmetric extraction of
 * future labor value via interest creates a Snare-like condition for those with
 * low power and no exit options.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_debt_utility,
    'Is the debt for a depreciating asset or a hidden productive utility (e.g., a car needed for a better job)?',
    'Empirical analysis of net income gain from the asset versus the total cost of interest over the loan term.',
    'If utility > cost, the constraint shifts toward a Rope for the Subject; if cost > utility, it confirms the Snare classification.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(consumer_debt_slavery, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows the normalization and intensification of consumer debt
% culture over the interval. Required for high-extraction constraints (E > 0.46).
%
% Theater ratio over time (Normalizing debt through lifestyle marketing):
narrative_ontology:measurement(cds_tr_t0, consumer_debt_slavery, theater_ratio, 0, 0.20).
narrative_ontology:measurement(cds_tr_t5, consumer_debt_slavery, theater_ratio, 5, 0.35).
narrative_ontology:measurement(cds_tr_t10, consumer_debt_slavery, theater_ratio, 10, 0.45).

% Extraction over time (Interest rate deregulation, fee accumulation):
narrative_ontology:measurement(cds_ex_t0, consumer_debt_slavery, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(cds_ex_t5, consumer_debt_slavery, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(cds_ex_t10, consumer_debt_slavery, base_extractiveness, 10, 0.72).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type (enables Boltzmann floor + complexity offset)
% Consumer credit is a primary mechanism for allocating capital resources.
narrative_ontology:coordination_type(consumer_debt_slavery, resource_allocation).

% Network relationships (structural influence edges)
% Consumer debt levels directly impact the ability to afford housing.
narrative_ontology:affects_constraint(consumer_debt_slavery, housing_affordability).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */