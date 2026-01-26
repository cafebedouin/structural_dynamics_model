% ============================================================================
% CONSTRAINT STORY: conversational_dogmas_interruption
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: "The Church of Interruption: A conversation with a Wizard"
% ============================================================================

:- module(constraint_conversational_dogmas_interruption, []).

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
 * constraint_id: conversational_dogmas_interruption
 * human_readable: Conversational Dogmas (Interruption vs. Strong Civility)
 * domain: social/technological
 * temporal_scope: Decades (Habits built over lifetimes)
 * spatial_scope: Global (Interpersonal interactions)
 * 
 * SUMMARY:
 * This constraint represents the rigid, often unconscious "dogmas" that govern 
 * human conversation. It contrasts the "Church of Interruption" (COI), which values
 * interruptions as signs of understanding, with the "Church of Strong Civility,"
 * which mandates silence and brevity. Misalignment leads to severe social friction.
 * 
 * KEY AGENTS:
 * - The Meek (Individual Powerless): Silenced or steamrolled by the COI member.
 * - The COI Member (Individual Moderate): Interrupts to signal understanding.
 * - The Corporate HR Department (Institutional): Manages internal communication protocols.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(conversational_dogmas_interruption, 0, 10).
narrative_ontology:constraint_claim(conversational_dogmas_interruption, tangled_rope).

% Base extractiveness: 0.3.
% The extraction is primarily of time, attention, and social 
% harmony. A COI member "muffs" conversations and offends others 
% unintentionally, extracting their patience.
domain_priors:base_extractiveness(conversational_dogmas_interruption, 0.3).

% Suppression: 0.6.
% Conversational habits are built over decades and are "harder... than 
% you think" to change. The alternative church's cues are often completely 
% invisible or misread.
domain_priors:suppression_score(conversational_dogmas_interruption, 0.6).

% Enforcement: Emerges naturally through decades of habit building.
domain_priors:emerges_naturally(conversational_dogmas_interruption).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(conversational_dogmas_interruption, intra_church_coordination).
constraint_victim(conversational_dogmas_interruption, inter_church_social_harmony).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE MEEK / THE CIVILIST - Snare
   --------------------------------------------------------------------------
   WHO: individual_powerless (Silenced or steamrolled by the COI member)
   WHEN: immediate (Short-term conversational frustration)
   WHERE: trapped (Caught in a "harder problem than you think" with no 
   neutral ground)
   
   WHY THIS CLASSIFICATION:
   For those outside the COI, being interrupted is a 'Snare'. It strangles 
   their expression and makes them "shut up tight." Their own cues for 
   politeness are turned against them, forcing them into silence or 
   offense.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    conversational_dogmas_interruption,
    snare,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE COI MEMBER (SAM) - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (Has agency within their own social circle)
   WHEN: biographical (Habits built over decades)
   WHERE: mobile (Can "happily converse with anyone else in [their] church")
   
   WHY THIS CLASSIFICATION:
   For a member of the Church of Interruption, the dogma is a 'Rope'—a highly 
   efficient coordination mechanism. It ensures understanding through immediate 
   feedback and summary, saving time for both parties within their own group.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    conversational_dogmas_interruption,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE CORPORATE HR DEPARTMENT - Tangled Rope
   --------------------------------------------------------------------------
   WHO: institutional (Manages internal communication protocols)
   WHEN: historical (Viewing decades of built-up habits in a workforce)
   WHERE: arbitrage (Attempts to implement training to bridge communication gaps)
   
   WHY THIS CLASSIFICATION:
   For a Corporate HR department, managing conversational dogmas is a 'Tangled Rope'.
   It's a 'Rope' because establishing clear communication protocols coordinates
   employees and improves efficiency. It's 'Tangled' because these deeply ingrained
   habits are hard to change, leading to constant friction and misunderstandings
   that degrade organizational harmony.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    conversational_dogmas_interruption,
    tangled_rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(arbitrage),
        spatial_scope(national)
    )
).

/* ==========================================================================
   4. TESTS
   ========================================================================== */

:- begin_tests(conversational_dogmas_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(conversational_dogmas_interruption, Type1, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(conversational_dogmas_interruption, Type2, context(agent_power(individual_moderate), _, _, _)),
    constraint_indexing:constraint_classification(conversational_dogmas_interruption, Type3, context(agent_power(institutional), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(conversational_dogmas_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'The Corporate HR Department' as the
 *    institutional agent. This highlights how these personal communication
 *    styles manifest as a management challenge, making it a 'Tangled Rope'
 *    for HR.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Meek (Snare): Silenced by misaligned conversational styles.
 *    - COI Member (Rope): Efficient within their own "church."
 *    - HR (Tangled Rope): Managing the friction caused by differing dogmas.
 * 
 * 3. CORE INSIGHT: Conversational dogmas are deeply ingrained 'Mountains' of
 *    habit. What is a highly efficient 'Rope' for one group can be a stifling
 *    'Snare' for another, creating a constant challenge for social harmony.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the plasticity of deeply ingrained conversational habits.
 */

omega_variable(
    conversational_style_plasticity,
    "Can conversational dogmas, built over decades, be truly unlearned (Rope), or are they permanent, 'hard-coded' transformations into one type of speaker (Mountain)?",
    resolution_mechanism("Long-term behavioral studies of individuals attempting to switch conversational 'churches', with neurological imaging of habit formation."),
    impact("If plastic: 'Rope' for social engineering. If fixed: 'Mountain' of cognitive architecture."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Turncoat / Hybrid Mastery (Mastering both dogmas)
 *    Viability: Sam wonders if the wizard has "mastered both dogmas."
 *    Suppression: Explicitly suppressed by the text's emphasis on the difficulty of changing habits built over "decades."
 *
 * CONCLUSION:
 * The text suggests that the apparent impossibility of changing deeply ingrained
 * conversational habits makes this constraint function as a 'Mountain'. The suppression
 * of "turncoat" alternatives highlights the rigidity of these social structures.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/conversational_dogmas_interruption].
 * 2. Multi-perspective: ?- multi_index_report(conversational_dogmas_interruption).
 * 3. Run tests: ?- run_tests(conversational_dogmas_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */