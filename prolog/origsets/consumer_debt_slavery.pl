% ============================================================================
% CONSTRAINT STORY: consumer_debt_slavery
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-03
% ============================================================================

:- module(constraint_consumer_debt, []).

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
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: consumer_debt_slavery
 * human_readable: The Interest Trap (Act Your Wage)
 * domain: economic
 * * SUMMARY:
 * A financial constraint where "poor people" utilize credit for depreciating 
 * assets, leading to a state described as "modern-day slavery." The extraction 
 * occurs through interest payments that consume the "fruits of labor" before 
 * the subject can build wealth.
 * * KEY AGENTS:
 * - The Borrower: Subject (Powerless) - Living beyond their "wage."
 * - The Creditor: Beneficiary (Institutional) - Entitled to the fruits of labor.
 * - The Financial Coach: Auditor (Analytical) - Advising "Acting your wage."
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(consumer_debt_slavery, 0.72). 
domain_priors:suppression_score(consumer_debt_slavery, 0.60).   
domain_priors:theater_ratio(consumer_debt_slavery, 0.45).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(consumer_debt_slavery, extractiveness, 0.72).
narrative_ontology:constraint_metric(consumer_debt_slavery, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(consumer_debt_slavery, theater_ratio, 0.45).

% Binary flags
domain_priors:requires_active_enforcement(consumer_debt_slavery).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% High extraction ($E=0.72$) felt as a loss of "freedom" and a predatory trap.
constraint_indexing:constraint_classification(consumer_debt_slavery, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Viewed as essential infrastructure for liquidity and "credit-driven growth."
constraint_indexing:constraint_classification(consumer_debt_slavery, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid nature: coordination (purchasing power) vs extraction (interest).
constraint_indexing:constraint_classification(consumer_debt_slavery, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(national))) :-
    domain_priors:base_extractiveness(consumer_debt_slavery, E), E >= 0.50,
    domain_priors:suppression_score(consumer_debt_slavery, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(debt_tests).

test(perspectival_gap) :-
    % Verify the Borrower sees a Snare while the Institutional Creditor sees a Rope.
    constraint_indexing:constraint_classification(consumer_debt_slavery, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(consumer_debt_slavery, rope, context(agent_power(institutional), _, _, _)).

test(extraction_intensity) :-
    domain_priors:base_extractiveness(consumer_debt_slavery, E),

    E > 0.46.

:- end_tests(debt_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction ($E=0.72$) is high because consumer debt on depreciating 
 * assets (cars, clothes, vacations) provides zero return on investment for the 
 * Subject while providing guaranteed income for the Beneficiary. 
 * * [RESOLVED MANDATROPHY]:
 * The "Modern-Day Slavery" claim is resolved by the Tangled Rope classification; 
 * while the system coordinates transactions, the asymmetric extraction of 
 * "labor fruits" via interest creates a snare for those with low power.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_debt_nature,
    'Is the asset truly depreciating or a hidden productive utility (e.g., commute for work)?',
    'Calculation of net income gain from the asset vs interest cost.',
    'If utility > cost: The constraint shifts toward a Rope for the Subject.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(consumer_debt_slavery, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time (Normalizing debt through lifestyle marketing)
narrative_ontology:measurement(cds_tr_t0, consumer_debt_slavery, theater_ratio, 0, 0.20).
narrative_ontology:measurement(cds_tr_t5, consumer_debt_slavery, theater_ratio, 5, 0.35).
narrative_ontology:measurement(cds_tr_t10, consumer_debt_slavery, theater_ratio, 10, 0.45).

% Extraction over time (Interest accumulation and "freedom" erosion)
narrative_ontology:measurement(cds_ex_t0, consumer_debt_slavery, base_extractiveness, 0, 0.40).
narrative_ontology:measurement(cds_ex_t5, consumer_debt_slavery, base_extractiveness, 5, 0.60).
narrative_ontology:measurement(cds_ex_t10, consumer_debt_slavery, base_extractiveness, 10, 0.72).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
