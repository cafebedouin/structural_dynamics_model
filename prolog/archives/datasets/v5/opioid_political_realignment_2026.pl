% ============================================================================
% CONSTRAINT STORY: opioid_political_realignment_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-03
% ============================================================================

:- module(constraint_opioid_realignment, []).

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
 * * constraint_id: opioid_political_realignment_2026
 * human_readable: Opioid-Induced Political Capture
 * domain: political/economic/social
 * * SUMMARY:
 * This constraint tracks the causal link between pharmaceutical extraction (opioid 
 * marketing) and subsequent political realignment in the United States. 
 * Quasi-exogenous exposure to the epidemic created economic hardship and dependency 
 * on public transfers, which crystallized into a durable Republican vote share 
 * increase (approx. 4.5 percentage points per std-dev increase by 2022).
 * * KEY AGENTS:
 * - [Affected Communities]: Subject (Powerless) - Trapped in cycles of mortality and economic decline.
 * - [Political Incumbents]: Beneficiary (Institutional) - Gaining additional House seats and vote share.
 * - [Litigants/Researchers]: Auditor (Analytical) - Uncovering the Purdue Pharma records and causal links.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% High extraction (0.68) due to the "quasi-exogenous" predatory marketing documented in records.
domain_priors:base_extractiveness(opioid_political_realignment_2026, 0.68). 

% High suppression (0.72) of alternatives; economic hardship restricts exit options for affected populations.
domain_priors:suppression_score(opioid_political_realignment_2026, 0.72).   

% Moderate theater (0.35) initially, as the "marketing" masqueraded as medicine (functional coordination).
domain_priors:theater_ratio(opioid_political_realignment_2026, 0.35).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(opioid_political_realignment_2026, extractiveness, 0.68).
narrative_ontology:constraint_metric(opioid_political_realignment_2026, suppression_requirement, 0.72).
narrative_ontology:constraint_metric(opioid_political_realignment_2026, theater_ratio, 0.35).

% Active enforcement not required; the constraint is maintained by addiction and political inertia.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The epidemic is a predatory trap. Withdrawal and economic ruin remove exit options.
constraint_indexing:constraint_classification(opioid_political_realignment_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Political parties view the realignment as a "natural" coordination around shared 
% grievances or "common sense" platform shifts, facilitating electoral success.
constraint_indexing:constraint_classification(opioid_political_realignment_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects the hybrid nature: a coordination around public transfers (Rope) 
% entangled with predatory pharmaceutical extraction (Snare).
constraint_indexing:constraint_classification(opioid_political_realignment_2026, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(opioid_political_realignment_2026, E), E >= 0.50,
    domain_priors:suppression_score(opioid_political_realignment_2026, S), S > 0.40.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(opioid_realignment_tests).

test(perspectival_gap) :-
    % Verify the Subject sees a Snare while the Beneficiary sees a Rope.
    constraint_indexing:constraint_classification(opioid_political_realignment_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(opioid_political_realignment_2026, rope, context(agent_power(institutional), _, _, _)).

test(drift_thresholds) :-
    domain_priors:base_extractiveness(opioid_political_realignment_2026, E),

    E > 0.46. % Triggers mandatory temporal data requirement.

:- end_tests(opioid_realignment_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.68) is anchored in the "quasi-exogenous" nature of the marketing—
 * it was not a response to existing pain, but a manufactured demand.
 * The Perspectival Gap is extreme: what the institutional agent views as a "realigned 
 * electorate" (coordination), the subject experiences as a "mortality trap" (extraction).
 *
 * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is necessary because the realignment isn't just 
 * random death; it's the creation of a new political feedback loop involving 
 * public transfer dependency. This creates a "forced coordination" function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% [RESOLVED MANDATROPHY] - The link is causal per litigation data.
omega_variable(
    omega_causality_depth,
    'To what extent is the realignment "sticky" if the pharmaceutical extraction is removed?',
    'Analysis of 2024-2030 voting patterns in post-settlement litigation zones.',
    'If vote share persists, it is a Mountain (Permanent Realignment); if it reverts, it was a Snare.',
    confidence_without_resolution(high)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(opioid_political_realignment_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Required for high-extraction (0.68 > 0.46). 
% Interval spans from early 2000s (T=0) to 2022 (T=10).

% Theater ratio: Increases as the medical justification for marketing 
% (functional) is replaced by the "theatrical" persistence of the political divide.
narrative_ontology:measurement(opioid_tr_t0, opioid_political_realignment_2026, theater_ratio, 0, 0.15).
narrative_ontology:measurement(opioid_tr_t5, opioid_political_realignment_2026, theater_ratio, 5, 0.25).
narrative_ontology:measurement(opioid_tr_t10, opioid_political_realignment_2026, theater_ratio, 10, 0.35).

% Extraction: Increases as the opioid exposure accumulates and 
% translates into "additional House seats" (political rent-seeking).
narrative_ontology:measurement(opioid_ex_t0, opioid_political_realignment_2026, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(opioid_ex_t5, opioid_political_realignment_2026, base_extractiveness, 5, 0.52).
narrative_ontology:measurement(opioid_ex_t10, opioid_political_realignment_2026, base_extractiveness, 10, 0.68).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
