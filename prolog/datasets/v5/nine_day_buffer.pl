% ============================================================================
% CONSTRAINT STORY: nine_day_buffer
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_nine_day_buffer, []).

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
 * * constraint_id: nine_day_buffer
 * human_readable: The Nine-Day/Nine-Meal Fragility Threshold
 * domain: infrastructure
 * * SUMMARY:
 * The "Nine-Day Buffer" represents the thin margin of stability provided by 
 * modern logistics. It is the constraint of physical survival (Mountain) 
 * masked as a coordination mechanism (Rope). When the system fails, it 
 * transitions from a supportive Scaffold into a lethal Snare.
 * * KEY AGENTS:
 * - The Urbanite: Subject (Powerless). Dependent on the grocery "tap."
 * - The Logistical Architect: Beneficiary (Institutional). Optimizes for efficiency.
 * - The Systems Auditor: Auditor (Analytical). Monitors the theater of stability.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% [RESOLVED MANDATROPHY]: Extraction is high (0.52) because the system extracts 
% efficiency (profit) by externalizing the risk of collapse onto the powerless.
domain_priors:base_extractiveness(nine_day_buffer, 0.52). 
domain_priors:suppression_score(nine_day_buffer, 0.85).   % High: No viable non-JIT alternatives for urban populations.
domain_priors:theater_ratio(nine_day_buffer, 0.72).      % Piton potential: Maintenance of "full shelves" via psychological signaling.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(nine_day_buffer, extractiveness, 0.52).
narrative_ontology:constraint_metric(nine_day_buffer, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(nine_day_buffer, theater_ratio, 0.72).
domain_priors:requires_active_enforcement(nine_day_buffer).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE URBANITE (SNARE)
% For the powerless, the buffer is a trap; they cannot exit the city or 
% the supply chain without starvation.
constraint_indexing:constraint_classification(nine_day_buffer, snare, 
    context(agent_power(powerless), 
            time_horizon(immediate), 
            exit_options(trapped), 
            spatial_scope(regional))).

% PERSPECTIVE 2: THE LOGISTICS FIRM (ROPE)
% Viewed as a pure coordination mechanism that maximizes resource distribution.
constraint_indexing:constraint_classification(nine_day_buffer, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% Detects that the coordination (Rope) is inextricably tied to the 
% extraction of safety margins for capital efficiency (Snare).
constraint_indexing:constraint_classification(nine_day_buffer, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(global))) :-
    domain_priors:base_extractiveness(nine_day_buffer, E), E >= 0.50,
    domain_priors:suppression_score(nine_day_buffer, S), S > 0.40.

% PERSPECTIVE 4: THE HISTORICAL AUDITOR (PITON)
% The "Nine Day" rule is often a theatrical figure; actual buffers may be 
% shorter, but the 9-day myth persists to prevent panic.
constraint_indexing:constraint_classification(nine_day_buffer, piton, 
    context(agent_power(analytical), 
            time_horizon(civilizational), 
            exit_options(arbitrage), 
            spatial_scope(universal))) :-
    domain_priors:theater_ratio(nine_day_buffer, TR), TR > 0.70.

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(nine_day_buffer_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(nine_day_buffer, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(nine_day_buffer, rope, context(agent_power(institutional), _, _, _)).

test(threshold_validation) :-
    domain_priors:base_extractiveness(nine_day_buffer, E),

    E >= 0.46. % Confirms high-extraction Snare/Tangled classification requirements.

:- end_tests(nine_day_buffer_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The Nine-Day Buffer is a classic "Tangled Rope." At the institutional level, 
 * it is a marvel of coordination ($E \approx 0$). However, for the individual, 
 * the high suppression of alternatives (urbanization) and the binary nature 
 * of the constraint (it either works or you starve) makes it a Snare ($E \ge 0.46$).
 * The Perspectival Gap exists because the institution perceives the "flow" 
 * while the individual perceives the "dependency."
 *
 * MANDATROPHY ANALYSIS:
 * Mandatrophy occurs here when the coordination function (feeding people) 
 * is cannibalized by the extraction function (cost-cutting). By labeling 
 * this as a Tangled Rope, we acknowledge that the system's "Rope" nature 
 * is what makes its "Snare" potential so dangerous.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_buffer_depth,
    'Is the 9-day threshold a biological constant or a technological variable?',
    'Empirical stress-testing of caloric density vs. transport speed.',
    'If biological, it is a Mountain; if technological, it is a Scaffold.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(nine_day_buffer, 0, 9). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
