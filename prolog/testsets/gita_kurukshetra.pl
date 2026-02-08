% ============================================================================
% CONSTRAINT STORY: gita_kurukshetra
% ============================================================================
% Generated: 2026-01-23
% Model: Gemini Pro (Revised)
% Source: The Bhagavad-Gita (Edwin Arnold Translation)
% ============================================================================

:- module(constraint_gita_kurukshetra, []).

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
 * constraint_id: gita_kurukshetra
 * human_readable: The Duty of the Kshatriya (Warrior Caste)
 * domain: religious/philosophical/social
 * temporal_scope: Ancient India (The Mahabharata era)
 * spatial_scope: The sacred plain of Kurukshetra
 * 
 * SUMMARY:
 * The "dharma" or sacred duty of the warrior caste (Kshatriya) to fight in a 
 * lawful war, even against kin. It presents an inescapable moral obligation 
 * rooted in the nature of the soul and the cosmic order, as articulated by Krishna.
 * 
 * KEY AGENTS:
 * - Arjuna (Individual Powerless): The Prince/Warrior, experiencing moral distress.
 * - Krishna (Institutional): The Supreme Being/Charioteer, who reframes the battle as cosmic necessity.
 * - The Enlightened Sage (Analytical): Observes the universal, unchangeable laws of existence.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(gita_kurukshetra, 0, 10).
narrative_ontology:constraint_claim(gita_kurukshetra, tangled_rope).

% Base extractiveness: 0.2.
% While a war extracts lives, the Gita argues that "Right Action" yields 
% no separate profit for the actor; it is for the "upholding of thy kind".
domain_priors:base_extractiveness(gita_kurukshetra, 0.2).

% Suppression: 0.9.
% Alternatives (like fleeing to be a beggar) are explicitly framed as 
% "shameful," "infamous," and "sinful" for a noble-born warrior.
domain_priors:suppression_score(gita_kurukshetra, 0.9).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(gita_kurukshetra, extractiveness, 0.2).
narrative_ontology:constraint_metric(gita_kurukshetra, suppression_requirement, 0.9).

% Enforcement: Emerges naturally from the cosmic order (Dharma).
domain_priors:emerges_naturally(gita_kurukshetra).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(gita_kurukshetra, cosmic_order).
constraint_victim(gita_kurukshetra, individual_ego).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: ARJUNA (INITIAL STATE) - Snare
   --------------------------------------------------------------------------
   WHO: powerless (Feels trapped by family ties and sin)
   WHEN: immediate (The horror of the battle to break tomorrow)
   WHERE: trapped (Sees "mutual slaughter" with no good outcome)
   
   WHY THIS CLASSIFICATION:
   Arjuna sees the constraint as extractive and destructive. It "perisheth" 
   households and leads to "Hell." It is a moral weight that paralyzes him,
   strangling his will to fight due to overwhelming grief and confusion.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    gita_kurukshetra,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: KRISHNA (THE TEACHER) - Rope
   --------------------------------------------------------------------------
   WHO: institutional (The Supreme Lord/Charioteer, guiding universal order)
   WHEN: historical (Spanning ages and "Kalpas")
   WHERE: mobile (Beyond the three qualities/modes)
   
   WHY THIS CLASSIFICATION:
   Krishna frames Dharma as a functional 'Rope'—a coordination mechanism
   for the cosmic order. "Work is more excellent than idleness." It is a path
   to "highest bliss" and "deliverance" when done without attachment, providing
   a means to fulfill one's role in the universe.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    gita_kurukshetra,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(mobile),
        spatial_scope(global)
    )
).

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE ENLIGHTENED SAGE - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (The observer of the "Qualities" and universal laws)
   WHEN: civilizational (The life of the Spirit is "eternal")
   WHERE: analytical (Not constrained by results)
   
   WHY THIS CLASSIFICATION:
   To the sage, Dharma is an unchangeable 'Mountain'—a natural law of the
   Spirit. "Life cannot slay. Life is not slain!" Actions are just the "play
   of visible things" and "Nature's way," an immutable truth of existence.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    gita_kurukshetra,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
).

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(gita_kurukshetra_tests).

test(multi_perspective_variance) :-
    constraint_indexing:constraint_classification(gita_kurukshetra, Type1, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(gita_kurukshetra, Type2, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(gita_kurukshetra, Type3, context(agent_power(analytical), _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3,
    Type1 \= Type3.

:- end_tests(gita_kurukshetra_tests).

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
 * 1. MANDATROPHY STATUS: The high suppression (0.9) indicates a powerful
 *    constraint. The "Tangled Rope" reflects the complexity of Dharma, which
 *    is both a guiding principle (Rope) and a potential source of moral
 *    distress (Snare).
 *
 * 2. CLASSIFICATION RATIONALE:
 *    - Arjuna (Snare): Overwhelmed by moral dilemma.
 *    - Krishna (Rope): Dharma as a tool for cosmic coordination.
 *    - Sage (Mountain): Dharma as an immutable universal law.
 * 
 * 3. CORE INSIGHT: The Bhagavad-Gita explores the 'Tangled Rope' of Dharma.
 *    What appears as a suffocating 'Snare' of duty to the individual (Arjuna)
 *    is reframed by the divine (Krishna) as a guiding 'Rope' for cosmic
 *    coordination and seen by the enlightened (Sage) as an immutable 'Mountain'
 *    of spiritual law.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */
/**
 * OMEGA IDENTIFICATION
 *
 * The core uncertainty is the interpretation of selfless action in a world of consequences.
 */

omega_variable(
    karma_yoga_efficacy,
    "Is 'Karma Yoga' (action without attachment to results) truly a universal 'Rope' for navigating moral dilemmas, or is its effectiveness dependent on a specific metaphysical worldview (Mountain)?",
    resolution_mechanism("Comparative analysis of ethical decision-making frameworks across diverse philosophical and religious traditions; psychological studies on the impact of detachment on well-being."),
    impact("If universal: 'Rope' for all. If worldview-dependent: A 'Tangled Rope' with limited applicability."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * VIABLE ALTERNATIVES
 *
 * ALTERNATIVE 1: Sannyas (Renunciation/Asceticism)
 *    Viability: Arjuna proposes this alternative, suggesting he would rather live on "beggar's bread" than fight.
 *    Suppression: Krishna vehemently rejects this, arguing that "none shall 'scape from act by shunning action" and redefines renunciation as giving up the *fruit* of action, not the action itself.
 *
 * CONCLUSION:
 * The Bhagavad-Gita actively suppresses the alternative of complete
 * renunciation (Sannyas) in favor of selfless action within one's Dharma.
 * This reframing turns the potential 'Snare' of inaction into a guiding
 * 'Rope' of purpose.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 
 * 1. Load: ?- [constraints/gita_kurukshetra].
 * 2. Multi-perspective: ?- multi_index_report(gita_kurukshetra).
 * 3. Run tests: ?- run_tests(gita_kurukshetra_tests).
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
domain_priors:theater_ratio(gita_kurukshetra, 0.34).
narrative_ontology:constraint_metric(gita_kurukshetra, theater_ratio, 0.34).

% ============================================================================
% ENRICHMENT: Structural predicates for dynamic classification
% Generated: 2026-02-08
% Template: v5.2 namespace alignment
% Source: Derived from existing narrative and structural content in this file
% ============================================================================

% --- Enforcement flag (required for tangled_rope gate) ---
% Tangled rope requires: constraint_beneficiary + constraint_victim + requires_active_enforcement
domain_priors:requires_active_enforcement(gita_kurukshetra).
