% ============================================================================
% CONSTRAINT STORY: semantic_overload_friction
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-01-28
% ============================================================================

:- module(constraint_semantic_overload, []).

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
 * * constraint_id: semantic_overload_friction
 * human_readable: The Semantic Saturation Threshold
 * domain: linguistics/professional_gatekeeping
 * * SUMMARY:
 * This constraint represents the "Friction of Jargon." As a specialized 
 * domain (e.g., law, tech, or academia) matures, its language becomes 
 * increasingly dense. While this facilitates precise internal coordination 
 * (Rope), it acts as a massive barrier to entry and a source of confusion 
 * (Snare) for external subjects.
 * * KEY AGENTS:
 * - The Layman: Subject (Powerless). Unable to decode the "Mandarin" language.
 * - The Professional Guild: Beneficiary (Institutional). Protects territory 
 * through linguistic complexity.
 * - The LLM/Translator: Auditor (Analytical). Quantifies the information 
 * density vs. comprehension gap.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% [RESOLVED MANDATROPHY]: Extraction (0.55) is high because the "Tax" 
% is paid in cognitive labor and time. The system extracts compliance 
% by making the rules unintelligible to those they govern.
domain_priors:base_extractiveness(semantic_overload_friction, 0.55). 
domain_priors:suppression_score(semantic_overload_friction, 0.90).   % High: No alternative language exists to participate in the domain.
domain_priors:theater_ratio(semantic_overload_friction, 0.45).      % Moderate: Some jargon is functional; some is performative.

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(semantic_overload_friction, extractiveness, 0.55).
narrative_ontology:constraint_metric(semantic_overload_friction, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(semantic_overload_friction, theater_ratio, 0.45).
domain_priors:requires_active_enforcement(semantic_overload_friction).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE LAYMAN (SNARE)
% For the non-specialist, the language is a Snare. They are trapped by 
% rules they cannot understand, requiring them to hire intermediaries.
constraint_indexing:constraint_classification(semantic_overload_friction, snare, 
    context(agent_power(powerless), 
            time_horizon(immediate), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE GUILD MEMBER (ROPE)
% To the professional, the jargon is a Rope. It is a high-bandwidth 
% coordination mechanism that allows for precision and shared identity.
constraint_indexing:constraint_classification(semantic_overload_friction, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE LINGUISTIC AUDITOR (TANGLED ROPE)
% Analytically, the system is a Tangled Rope. The very tool used for 
% coordination (specialized terms) is the same tool used for extraction 
% (gatekeeping).
constraint_indexing:constraint_classification(semantic_overload_friction, tangled_rope, 
    context(agent_power(analytical), 
            time_horizon(historical), 
            exit_options(analytical), 
            spatial_scope(universal))) :-
    domain_priors:base_extractiveness(semantic_overload_friction, E), E > 0.46.

% PERSPECTIVE 4: THE REFORMER (SCAFFOLD)
% During a "Plain Language" reform, the complex terminology is treated 
% as a Scaffold—a legacy support system to be dismantled over time.
constraint_indexing:constraint_classification(semantic_overload_friction, scaffold, 
    context(agent_power(organized), 
            time_horizon(immediate), 
            exit_options(constrained), 
            spatial_scope(national))) :-
    narrative_ontology:has_sunset_clause(semantic_overload_friction).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(semantic_overload_tests).

test(tangled_rope_signature) :-
    % Verify that high-extraction language is flagged as a Tangled Rope.
    constraint_indexing:constraint_classification(semantic_overload_friction, tangled_rope, context(agent_power(analytical), _, _, _)).

test(gatekeeping_gap) :-
    % Verify the tension between the insider (Rope) and the outsider (Snare).
    constraint_indexing:constraint_classification(semantic_overload_friction, rope, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(semantic_overload_friction, snare, context(agent_power(powerless), _, _, _)).

:- end_tests(semantic_overload_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * Semantic Overload is a "Hidden Snare." Because language is usually seen 
 * as a public good (Rope), its use as a tool of exclusion ($E = 0.55$) 
 * often goes unchallenged. 
 * The Perspectival Gap exists because insiders view the "precision" 
 * as the benefit, while outsiders view the "obscurity" as the cost.
 *
 * MANDATROPHY ANALYSIS:
 * This represents "Functional Decay." When the jargon no longer maps 
 * to reality but exists only to maintain the Guild's status, it 
 * transitions from a Tangled Rope into a Piton.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_jargon_utility,
    'Is the complexity required for technical precision, or is it purely performative?',
    'Information-theoretic analysis of document length vs. semantic density.',
    'If purely performative, the Tangled Rope becomes a Piton.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(semantic_overload_friction, 0, 1). % Comprehension coefficient.

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
