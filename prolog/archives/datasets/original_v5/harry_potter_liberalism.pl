% ============================================================================
% CONSTRAINT STORY: HARRY_POTTER_LIBERALISM
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-26
% ============================================================================

:- module(constraint_hp_liberalism, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: hp_liberalism
 * human_readable: The Potterverse Liberalism Constraint
 * domain: socio_political
 * * SUMMARY:
 * This constraint represents the "Mirror of Erised" effect where 1990s liberal
 * virtues (tolerance, non-violence, institutional trust) act as a coordination
 * mechanism for Millennials but are perceived as an extractive or atrophied
 * trap by Zoomers facing economic decline.
 * * KEY AGENTS:
 * - Millennial Believer: Subject (Powerless/Trapped in nostalgia)
 * - Warner Bros / IP Holders: Beneficiary (Institutional/Mobile)
 * - The Zoomer Skeptic: Auditor (Analytical/Seeking Exit)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is high (0.52) due to corporate monetization of nostalgia despite
% waning socio-political utility for the youngest cohort.
domain_priors:base_extractiveness(hp_liberalism, 0.52).
domain_priors:suppression_score(hp_liberalism, 0.45).
domain_priors:theater_ratio(hp_liberalism, 0.78).       % High "theatrical" maintenance in spinoffs

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(hp_liberalism, extractiveness, 0.52).
narrative_ontology:constraint_metric(hp_liberalism, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(hp_liberalism, theater_ratio, 0.78).

% Binary flags
domain_priors:requires_active_enforcement(hp_liberalism). % Required to maintain "canon" moral guides.

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE MILLENNIAL "HP ADULT" (ROPE/MOUNTAIN)
% Viewed as a foundational moral guide (Mountain) or essential social coordination (Rope).
constraint_indexing:constraint_classification(hp_liberalism, rope,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(national))).

% PERSPECTIVE 2: THE IP BENEFICIARY / WARNER BROS (TANGLED ROPE)
% Maintains the coordination (Rope) while extracting massive capital (Hogwarts Legacy sales).
constraint_indexing:constraint_classification(hp_liberalism, tangled_rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ZOOMER SKEPTIC (PITON)
% Sees the constraint as a non-functional, inertial remnant of an affluent era that no longer exists.
constraint_indexing:constraint_classification(hp_liberalism, piton,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(arbitrage),
            spatial_scope(national))) :-
    domain_priors:theater_ratio(hp_liberalism, TR), TR > 0.70.

% PERSPECTIVE 4: THE RELATIVIST CRITIC (SNARE)
% Sees the 90s liberalism as a trap that suppressed more radical/necessary political responses.
constraint_indexing:constraint_classification(hp_liberalism, snare,
    context(agent_power(organized),
            time_horizon(biographical),
            exit_options(constrained),
            spatial_scope(regional))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(hp_liberalism_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Piton/Snare for the analytical/organized but a Rope for the powerless millennial.
    constraint_indexing:constraint_classification(hp_liberalism, rope, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(hp_liberalism, piton, context(agent_power(analytical), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(hp_liberalism, E),

    E >= 0.46. % Confirms it's a high-extraction Tangled Rope or Snare.

:- end_tests(hp_liberalism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The "Perspectival Gap" arises from the mismatch between the 1990s material
 * conditions (peaceful, affluent, high-trust) and the 2020s reality (stagnant
 * wages, polarization). For Millennials, the constraint is a 'Rope' because
 * it coordinated their entry into adulthood. For Zoomers, it is a 'Piton'—
 * a spike driven into a wall that no longer supports the weight of their
 * political reality.
 * * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The system prevents mislabeling this as pure extraction (Snare) by
 * acknowledging its 'Tangled Rope' status at the institutional level. While
 * Warner Bros extracts (E=0.52), the narrative still provides genuine
 * coordination for 40M+ players, preventing the classification from
 * collapsing into a 'Snare' for all indices.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_hp_resilience,
    'Will the HBO adaptation successfully re-index the constraint as a Rope for Zoomers?',
    'Market share analysis of the under-18 demographic upon 2026 release.',
    'Success = Reversion to Rope; Failure = Hard pivot to Piton/Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(hp_liberalism, 1997, 2026).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
