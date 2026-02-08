% ============================================================================
% CONSTRAINT STORY: extraordinary_narrative_shift
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: "The Extraordinary Intruding on the Ordinary" by cafebedouin.org
% ============================================================================

:- module(constraint_extraordinary_narrative_shift, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * 
 * constraint_id: extraordinary_narrative_shift
 * human_readable: The Story of the Extraordinary
 * domain: social/psychological/cultural
 * temporal_scope: Perennial / Historical
 * spatial_scope: Global / Cognitive
 * 
 * SUMMARY:
 * The distinction between "extraordinary" and "ordinary" is not a physical property but a function of frequency, 
 * quantity, and the narrative framing we apply to experience. The constraint identifies that 
 * repetitive, commonplace existence truly defines our experience, and "extraordinary" moments are simply 
 * ordinary ones with a different story attached.
 * 
 * KEY AGENTS:
 * - The Sherpa (Individual Powerless): Repetitive laborer whose work is framed as "extraordinary" by others.
 * - Media Organization / Tourism Industry (Institutional): Crafts "extraordinary" narratives for audiences.
 * - Bernard Moitessier (Individual Moderate): A sailor who abandoned an "extraordinary" race to live an "ordinary" journey.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(extraordinary_narrative_shift, 0, 10).
narrative_ontology:constraint_claim(extraordinary_narrative_shift, tangled_rope).

% Base extractiveness: 0.4.
% Narrative framing "extracts" the raw, lived presence of the moment to serve a "branding" 
% or "unique" story. It also extracts labor (Sherpa) for the sake of another's "extraordinary" story.
domain_priors:base_extractiveness(extraordinary_narrative_shift, 0.4).

% Suppression: 0.4.
% The "extraordinary" narrative suppresses the value of the "ordinary" repetition, 
% making the bulk of life feel like it doesn't "matter" unless it fits a specific narrative.
domain_priors:suppression_score(extraordinary_narrative_shift, 0.4).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(extraordinary_narrative_shift, extractiveness, 0.4).
narrative_ontology:constraint_metric(extraordinary_narrative_shift, suppression_requirement, 0.4).

% Enforcement: Emerges naturally from human storytelling and cognitive frequency bias.
domain_priors:emerges_naturally(extraordinary_narrative_shift).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(extraordinary_narrative_shift, the_storyteller).
constraint_victim(extraordinary_narrative_shift, lived_experience).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE SHERPA (REPETITIVE LABOR) - Snare
   --------------------------------------------------------------------------
   WHO: powerless (Subject to the frequency and volume of work)
   WHEN: immediate (Daily repetitive task)
   WHERE: trapped (Bound by the physical necessity of the mountain)
   
   WHY THIS CLASSIFICATION:
   For the Sherpa, the "extraordinary" summit of Everest is a 'Snare'. The narrative
   of the tourist's "one minute at the pinnacle" extracts the Sherpa's daily
   labor while rendering their experience invisible to the "Extraordinary" story.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    extraordinary_narrative_shift,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: MEDIA ORGANIZATION / TOURISM INDUSTRY - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Crafts "extraordinary" narratives for audiences)
   WHEN: biographical (The lifecycle of a marketing campaign or news story)
   WHERE: arbitrage (Transforms ordinary events into marketable narratives)
   
   WHY THIS CLASSIFICATION:
   For a media organization or tourism industry, crafting "extraordinary"
   narratives is a 'Rope'—a powerful tool for attracting audiences and customers.
   They transform ordinary events into unique, desirable experiences, driving
   engagement and revenue.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    extraordinary_narrative_shift,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: BERNARD MOITESSIER (THE SOLO SAILOR) - Rope
   --------------------------------------------------------------------------
   WHO: individual_moderate (Has agency to abandon the race)
   WHEN: biographical (Changing his personal life story mid-journey)
   WHERE: mobile (Abandons the competitive race for the experience)
   
   WHY THIS CLASSIFICATION:
   For Moitessier, the "Ordinary" repetition of sailing becomes a 'Rope'. He rejects
   the extractive race narrative to coordinate his existence with the "journey"
   rather than the "pinnacle," making his experience a functional tool for selfhood.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    extraordinary_narrative_shift,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(extraordinary_narrative_shift_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(extraordinary_narrative_shift, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(extraordinary_narrative_shift, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(extraordinary_narrative_shift, Type3, context(agent_power(individual_moderate), _, _, _)),
    Type1 \= Type2,
    Type1 \= Type3.

:- end_tests(extraordinary_narrative_shift_tests).

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
 * 1. INSTITUTIONAL PERSPECTIVE: Added 'Media Organization / Tourism Industry'
 *    as the institutional agent. For them, crafting "extraordinary" narratives
 *    is a powerful 'Rope' for profit.
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Sherpa (Snare): Labor extracted for another's "extraordinary" story.
 *    - Media/Tourism (Rope): A tool for attracting audiences and customers.
 *    - Moitessier (Rope): Rejects a competitive "Snare" for an ordinary "Rope".
 * 
 * 3. CORE INSIGHT: The concept of the "extraordinary" is a 'Tangled Rope'.
 *    It can be a 'Rope' for personal transformation (Moitessier) or commercial
 *    gain (media), but it can also become a 'Snare' for those whose ordinary
 *    labor is extracted to create another's "extraordinary" moment (Sherpa).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the threshold at which "extraordinary" becomes "ordinary".
 */

omega_variable(
    repetition_threshold,
    "What is the specific 'frequency, quantity and volume' threshold where an extraordinary experience permanently collapses into the ordinary?",
    resolution_mechanism("Neurological monitoring of dopamine response during repeated high-novelty stimuli; longitudinal studies of professional athletes and performers."),
    impact("If low threshold: 'Extraordinary' is a fragile 'Rope'. If high threshold: It is a durable 'Mountain' of experience."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Identifying with the Ordinary
 *    Viability: Moitessier's choice proves this is a real alternative to the pursuit of "extraordinary" achievements.
 *    Suppression: Actively suppressed by cultural narratives that glorify "unique" accomplishments and "peak experiences".
 *
 * CONCLUSION:
 * The cultural pressure to achieve the "extraordinary" creates a 'Snare' for
 * many, extracting their well-being in the pursuit of an often-elusive narrative.
 * The alternative of identifying with the ordinary, as Moitessier did, offers a
 * powerful 'Rope' for navigating life with greater authenticity.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/extraordinary_narrative_shift].
 * 2. Multi-perspective: ?- multi_index_report(extraordinary_narrative_shift).
 * 3. Run tests: ?- run_tests(extraordinary_narrative_shift_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */

% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Multifile declarations for new predicates ---
:- multifile
    domain_priors:theater_ratio/2.

% --- Theater ratio (missing from base properties) ---
% Mixed coordination/extraction — theater masks extraction component
domain_priors:theater_ratio(extraordinary_narrative_shift, 0.31).
narrative_ontology:constraint_metric(extraordinary_narrative_shift, theater_ratio, 0.31).

% --- Analytical perspective classification (missing) ---
% chi = 0.4 * 1.15 (analytical) * 1.2 (global) = 0.552
% Classification: tangled_rope
constraint_indexing:constraint_classification(extraordinary_narrative_shift, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Enforcement flag (required for tangled_rope gate) ---
% Tangled rope requires: constraint_beneficiary + constraint_victim + requires_active_enforcement
domain_priors:requires_active_enforcement(extraordinary_narrative_shift).
