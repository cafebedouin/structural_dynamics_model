% ============================================================================
% CONSTRAINT STORY: frankenstein_creation_hubris
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: Frankenstein; or, The Modern Prometheus by Mary Shelley
% Status: [RESOLVED MANDATROPHY]
% ============================================================================

:- module(constraint_frankenstein_creation_hubris, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: frankenstein_creation_hubris
 * human_readable: The Prometheus/Creator Burden
 * domain: technological/existential/social
 * temporal_scope: Late 18th Century / Scientific Revolution
 * spatial_scope: Europe (Geneva, Ingolstadt, Britain, Arctic)
 * 
 * SUMMARY:
 * Victor Frankenstein successfully animates matter, creating a sentient being. 
 * The constraint arises from the irreversible nature of this creation and the 
 * absolute moral and physical obligation the creator has toward the creature—
 * an obligation Victor rejects, triggering a catastrophic cycle of vengeance.
 * 
 * KEY AGENTS:
 * - The Creature (Individual Powerless): A "biological artifact" with zero social standing.
 * - Scientific Community / Ethics Board (Institutional): Establishes and enforces ethical boundaries in research.
 * - Victor Frankenstein (Individual Powerful): The creator who rejects his obligation.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(frankenstein_creation_hubris, 0, 10).
narrative_ontology:constraint_claim(frankenstein_creation_hubris, snare).

% Base extractiveness: 0.9.
% The creation extracts the life, sanity, and loved ones of the creator.
% It also extracts the possibility of social existence from the creature.
domain_priors:base_extractiveness(frankenstein_creation_hubris, 0.9).

% Suppression score: 0.85.
% The secret of the creation is suppressed by Victor’s ego and fear, 
% preventing any intervention until it is too late.
domain_priors:suppression_score(frankenstein_creation_hubris, 0.85).

% Enforcement: The constraint is enforced by the physical capabilities of 
% the creature and the psychological guilt of the creator.
domain_priors:requires_active_enforcement(frankenstein_creation_hubris).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(frankenstein_creation_hubris, scientific_knowledge).
constraint_victim(frankenstein_creation_hubris, frankenstein_lineage).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE CREATURE - Mountain
   --------------------------------------------------------------------------
   WHO: individual_powerless (Abandoned at birth, no legal or social status)
   WHEN: immediate (Every waking moment is defined by physical rejection)
   WHERE: trapped (Bound by a hideous form that precludes any human exit)
   
   WHY THIS CLASSIFICATION:
   For the Creature, the laws of human social rejection are as immutable 
   as a 'Mountain'. He cannot negotiate his appearance or the instinctive 
   horror people feel. It appears to him as a natural law of misery, a fixed
   reality from which there is no escape.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    frankenstein_creation_hubris,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: SCIENTIFIC COMMUNITY / ETHICS BOARD - Mountain
   --------------------------------------------------------------------------
   WHO: institutional (Establishes and enforces ethical boundaries in research)
   WHEN: historical (Drawing lessons from scientific hubris across generations)
   WHERE: analytical (Observes the long-term consequences of unchecked ambition)
   
   WHY THIS CLASSIFICATION:
   For the scientific community or an ethics board, Frankenstein's story is a
   'Mountain'—a cautionary tale of the inherent risks and ethical boundaries
   in scientific creation. It represents an immutable truth about the
   responsibilities of creators and the societal consequences of hubris,
   serving as a fixed point of reference for ethical guidelines.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    frankenstein_creation_hubris,
    mountain,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: VICTOR FRANKENSTEIN - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerful (Possesses the knowledge and wealth to create life)
   WHEN: biographical (The secret haunts him from Ingolstadt to his death)
   WHERE: constrained (The Creature haunts his movements across Europe)
   
   WHY THIS CLASSIFICATION:
   Initially, Victor sees science as a Rope to pull him toward glory. 
   Once the Creature is animated and abandoned, it becomes a 'Snare'—a coercive 
   force that extracts his peace and the lives of those he loves, strangling
   his happiness and ultimately leading to his demise.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    frankenstein_creation_hubris,
    snare,
    context(
        agent_power(individual_powerful),
        time_horizon(biographical),
        exit_options(constrained),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(frankenstein_creation_hubris_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(frankenstein_creation_hubris, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(frankenstein_creation_hubris, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(frankenstein_creation_hubris, Type3, context(agent_power(individual_powerful), _, _, _)),
    Type1 \= Type3. % Creature (Mountain) vs Victor (Snare)

:- end_tests(frankenstein_creation_hubris_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * 
 * Model: Gemini Pro (Revised)
 * Date: 2026-01-23
 * 
 * KEY DECISIONS:
 * 
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'Scientific Community / Ethics Board' as
 *    the institutional agent. For them, Frankenstein's story is a 'Mountain',
 *    a permanent cautionary tale in the pursuit of knowledge.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - The Creature (Mountain): Immutable social rejection due to appearance.
 *    - Scientific Community (Mountain): Cautionary tale of ethical boundaries.
 *    - Victor Frankenstein (Snare): His creation extracts his peace and life.
 * 
 * 3. CORE INSIGHT: Frankenstein's story reveals a profound 'Mountain' of ethical
 *    responsibility inherent in creation. Victor's hubris turns his scientific
 *    'Rope' (the pursuit of knowledge) into a fatal 'Snare' for himself and
 *    his loved ones, while the Creature remains trapped by an immutable social 'Mountain'.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * Mandatory Omega for high-extraction constraints.
 */

omega_variable(
    paternal_virtue_vs_physical_noose,
    "Would a benevolent upbringing and acceptance have overridden the Creature's physical 'Mountain' of deformity and the social 'Snare' of rejection, transforming him into a benevolent 'Rope' for society?",
    resolution_mechanism("Comparative narrative analysis with modern psychological developmental models on the impact of nurture versus nature; ethical thought experiments on creator responsibility."),
    impact("If yes: The tragedy is social/moral and avoidable. If no: The tragedy is biological/fate and inevitable."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Scientific Paternalism (Raising the Creature with care)
 *    Viability: Victor possessed the means to raise the creature in isolation and educate him.
 *    Suppression: Actively suppressed by Victor's "horror and disgust," which immediately followed the creation, turning his initial scientific 'Rope' into a 'Snare' of abandonment.
 *
 * CONCLUSION:
 * The tragedy of Frankenstein lies in Victor's active suppression of his
 * ethical alternatives. His failure to adopt "scientific paternalism"
 * (a potential 'Rope') created a permanent 'Snare' for himself and
 * an unchangeable 'Mountain' of social rejection for his Creature.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/frankenstein_creation_hubris].
 * 2. Multi-perspective: ?- multi_index_report(frankenstein_creation_hubris).
 * 3. Run tests: ?- run_tests(frankenstein_creation_hubris_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */