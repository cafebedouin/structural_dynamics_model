% ============================================================================
% CONSTRAINT STORY: EVOLUTIONARY_KNOWLEDGE
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-26
% ============================================================================

:- module(constraint_evolution_epistemic, []).

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
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: evolutionary_knowledge
 * human_readable: The Primordial Pain-Epistemic Constraint
 * domain: biological/philosophy
 * * SUMMARY:
 * Knowledge is an evolved biological adaptation rooted in the sensation of 
 * pain. This proto-knowledge (subjective) precedes objective knowledge of 
 * space/time and acts as an irreducible limit on how life interprets reality.
 * * KEY AGENTS:
 * - The Primitive Organism: Subject (Powerless vs. Pain)
 * - Natural Selection: Beneficiary (Institutional/Evolutionary Coordination)
 * - The Epistemic Scientist: Auditor (Analytical/Diagnostic)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
% Extraction is low (0.08) as this is a natural adaptation for survival, 
% though it requires the "struggle" of the organism.
domain_priors:base_extractiveness(evolutionary_knowledge, 0.08). 
domain_priors:suppression_score(evolutionary_knowledge, 0.65).   % Pain "shapes all later elaborations"
domain_priors:theater_ratio(evolutionary_knowledge, 0.10).       % High functional/biological utility.

% Binary flags
domain_priors:requires_active_enforcement(evolutionary_knowledge). % Maintained by the "Cruel Gene."

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE PRIMITIVE ORGANISM (MOUNTAIN)
% For the single cell or swamp-dweller, pain is an irreducible physical law 
% and a mandatory information signal with zero degrees of freedom.
constraint_indexing:constraint_classification(evolutionary_knowledge, mountain, 
    context(agent_power(individual_powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(local))).

% PERSPECTIVE 2: NATURAL SELECTION / THE SPECIES (ROPE)
% Viewed as a master coordination mechanism (survival of the painiest) 
% that allows the lineage to navigate damage and danger across eons.
constraint_indexing:constraint_classification(evolutionary_knowledge, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE "SUPER-SCIENTIST" AUDITOR (PITON)
% The history of knowledge is used as an analytical hook (Piton) into 
% understanding advanced degrees and diplomas as iterations of primordial pangs.
constraint_indexing:constraint_classification(evolutionary_knowledge, piton, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(civilizational))).

% PERSPECTIVE 4: THE UNIVERSITY / ACADEMIA (TANGLED ROPE)
% Coordinates the "love of wisdom" but extracts labor through the 
% "struggle" and "hardness" of knowledge acquisition.
constraint_indexing:constraint_classification(evolutionary_knowledge, tangled_rope, 
    context(agent_power(organized), 
            time_horizon(biographical), 
            exit_options(constrained), 
            spatial_scope(national))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(evolution_epistemic_tests).

test(perspectival_gap) :-
    % Verify the constraint is a Mountain for the organism but a Rope for the evolutionary process.
    constraint_indexing:constraint_classification(evolutionary_knowledge, mountain, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(evolutionary_knowledge, rope, context(agent_power(institutional), _, _, _)).

test(threshold_adherence) :-
    domain_priors:base_extractiveness(evolutionary_knowledge, E),
    E < 0.10. % Validates its status as a "biological law" rather than a predatory snare.

:- end_tests(evolution_epistemic_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The low extraction score (0.08) reflects the essay's Darwinian spirit: 
 * knowledge is a "marvelous aid to survival." The Perspectival Gap is 
 * fundamental—the subject feels only the "Ouch" (Mountain), while 
 * selection "sees it coming" (Rope).
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * Labeling this as a 'Tangled Rope' for academia prevents the system from 
 * ignoring that modern knowledge still requires the "pain and struggle" 
 * of its ancestors, even if the biological "Mountain" has been refined 
 * into a cultural "accessory."
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_pain_priority,
    'Is pain truly the first form of consciousness, or is it a Snare of evolutionary over-interpretation?',
    'Comparative neuro-ethology of primitive organisms to find "knowledge" without nociception.',
    'Pain-First = Universal Epistemic DNA; Pleasure/Other-First = Fragmented Epistemic origins.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
% Covers the period from primordial "swampy creatures" to the 2026 essay.
narrative_ontology:interval(evolutionary_knowledge, -4000000000, 2026). 

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
