% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: dionysiac_frenzy
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: The Bacchae of Euripides (Gilbert Murray translation)
% ============================================================================

:- module(constraint_dionysiac_frenzy, []).

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
 * * constraint_id: dionysiac_frenzy
 * human_readable: Dionysiac Religious Possession
 * domain: religious/social
 * temporal_scope: Mythic/Classical Antiquity (Thebes)
 * spatial_scope: Regional (Thebes and Mt. Kithaeron)
 * * SUMMARY:
 * A state of divinely induced madness (ecstasy) that compels subjects to 
 * abandon social roles (weaving, child-rearing) for mountain rites. 
 * It functions as an irresistible force that punishes denial through 
 * catastrophic loss of agency and involuntary violence.
 * * KEY AGENTS:
 * - Pentheus: King of Thebes, attempts to suppress the cult through force.
 * - Agave: Mother of Pentheus, involuntarily possessed, kills her own son.
 * - Dionysus: The God/Constraint-source, enforces the frenzy as "recompense."
 * * NARRATIVE ARC:
 * Dionysus brings the frenzy to Thebes as punishment for the city's denial 
 * of his godhead. Pentheus attempts to "trap" the God and his followers, 
 * but is himself ensnared, "hypnotized" into cross-dressing, and led to 
 * Kithaeron where his possessed mother tears him apart, believing him to 
 * be a lion.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Required for [STEP 1] and [STEP 2] of the DR-Audit Suite
narrative_ontology:interval(dionysiac_frenzy_interval, 0, 10).
narrative_ontology:constraint_claim(dionysiac_frenzy, mountain).

% Base extractiveness score: High asymmetry in benefit/suffering
% Rationale: The God extracts total submission and physical life; the 
[cite_start]% possessed lose everything, while the "God" gains glory[cite: 115, 1240].
domain_priors:base_extractiveness(dionysiac_frenzy, 0.8).

% Suppression score: High
% Rationale: Alternatives (traditional civic life) are physically "stung" 
[cite_start]% away by the God's rod; dissenters are blinded or maddened[cite: 105, 249].
domain_priors:suppression_score(dionysiac_frenzy, 0.9).

% Enforcement requirements: Emerges naturally from divine presence
domain_priors:emerges_naturally(dionysiac_frenzy).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(dionysiac_frenzy, extractiveness, 0.8).
narrative_ontology:constraint_metric(dionysiac_frenzy, suppression_requirement, 0.9).

% Beneficiaries and Victims
constraint_beneficiary(dionysiac_frenzy, dionysus).
constraint_victim(dionysiac_frenzy, [pentheus, agave, cadmus]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================= */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: AGAVE / THE MAENADS - Mountain
   --------------------------------------------------------------------------
   
   [cite_start]WHO: individual_powerless - Stung by the rod, driven "wild-eyed"[cite: 105].
   [cite_start]WHEN: immediate - Experience is a total, present-tense "swoon"[cite: 184].
   [cite_start]WHERE: trapped - "Magic has goaded" them out; they cannot return until the God allows[cite: 108].
   SCOPE: regional - Limited to Kithaeron/Thebes.
   
   WHY THIS CLASSIFICATION:
   For those possessed, the frenzy is an unchangeable law of nature, as 
   unavoidable as an earthquake or the sunrise. They have zero degrees of 
   [cite_start]freedom to resist the "magic"[cite: 108, 168].
   
   NARRATIVE EVIDENCE:
   [cite_start]"Hath this my magic goaded out"[cite: 108]; [cite_start]"By the magic of his breath borne away"[cite: 168].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dionysiac_frenzy,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        constraint_beneficiary(dionysiac_frenzy, dionysus),
        constraint_victim(dionysiac_frenzy, agave),
        spatial_scope(regional)
    )
) :-
    domain_priors:suppression_score(dionysiac_frenzy, S),
    S > 0.8,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: DIONYSUS / THE GOD - Rope
   --------------------------------------------------------------------------
   
   [cite_start]WHO: institutional - Rule-making power, "He who ordained his shape"[cite: 529].
   [cite_start]WHEN: historical/civilizational - The God thinks in terms of "all the world"[cite: 93].
   [cite_start]WHERE: mobile - "Then to another land... must I fare onward"[cite: 116].
   [cite_start]SCOPE: global - "Asia all... and now I come to Hellas"[cite: 92, 93].
   
   WHY THIS CLASSIFICATION:
   [cite_start]To Dionysus, the frenzy is a functional "harness of my rites" [cite: 107] 
   [cite_start]used to "show me in men's sight"[cite: 93]. It is a tool for coordination 
   [cite_start]and recognition, changeable and directed by his will[cite: 529].
   
   NARRATIVE EVIDENCE:
   [cite_start]"I have bound upon the necks of them / The harness of my rites"[cite: 107].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dionysiac_frenzy,
    rope,
    context(
        agent_power(institutional),
        time_horizon(historical),
        exit_options(mobile),
        constraint_beneficiary(dionysiac_frenzy, dionysus),
        constraint_victim(dionysiac_frenzy, []),
        spatial_scope(global)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: PENTHEUS - Noose
   --------------------------------------------------------------------------
   
   [cite_start]WHO: individual_powerful - King of Thebes, "whom I have raised to rule"[cite: 265].
   [cite_start]WHEN: immediate - Reacts to the "strange rumour"[cite: 271].
   [cite_start]WHERE: constrained - He tries to "hunt" and "snare," but finds himself trapped[cite: 281, 1066].
   [cite_start]SCOPE: local - Focused on his "towers" and "walls"[cite: 1209].
   
   WHY THIS CLASSIFICATION:
   [cite_start]Pentheus sees the frenzy as an extractive, coercive "contagion" [cite: 366] 
   [cite_start]that steals his labor (wives/sisters) and threatens his power[cite: 271]. 
   [cite_start]It is asymmetric—it benefits a "stranger" at his expense[cite: 282, 372].
   
   NARRATIVE EVIDENCE:
   [cite_start]"Preying on our maids and wives"[cite: 372]; [cite_start]"Our wives... from their hearths are flown"[cite: 271].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    dionysiac_frenzy,
    noose,
    context(
        agent_power(individual_powerful),
        time_horizon(immediate),
        exit_options(constrained),
        constraint_beneficiary(dionysiac_frenzy, dionysus),
        constraint_victim(dionysiac_frenzy, pentheus),
        spatial_scope(local)
    )
) :-
    domain_priors:base_extractiveness(dionysiac_frenzy, E),
    E > 0.5,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(dionysiac_frenzy_tests).

test(multi_perspective_frenzy) :-
    constraint_indexing:constraint_classification(dionysiac_frenzy, Type1, context(agent_power(individual_powerless), _, _, _, _, _)),
    constraint_indexing:constraint_classification(dionysiac_frenzy, Type2, context(agent_power(institutional), _, _, _, _, _)),
    constraint_indexing:constraint_classification(dionysiac_frenzy, Type3, context(agent_power(individual_powerful), _, _, _, _, _)),
    Type1 \= Type2,
    Type2 \= Type3.

test(powerless_extraction) :-
    % Powerless (Agave) experience 0.8 extraction, while God experiences 0.0
    domain_priors:base_extractiveness(dionysiac_frenzy, 0.8).

test(time_immutability_frenzy) :-
    % Short term = Mountain for Agave, Long term = Rope for Dionysus' global plan
    true.

:- end_tests(dionysiac_frenzy_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS SCORE (0.8): The flow of value is almost entirely 
 * one-way. Agave loses her son and her home; Pentheus loses his life. 
 * [cite_start]The God receives "due meed of majesty" [cite: 338] and establishes 
 * his cult.
 * 2. PERSPECTIVE SELECTION: Chose Agave (Powerless), Dionysus (Institutional), 
 * and Pentheus (Powerful) to show the full Mountain-Rope-Noose spectrum.
 * 3. CLASSIFICATION RATIONALE:
 * [cite_start]Agave -> Mountain: She is literally a biological puppet[cite: 1368].
 * Dionysus -> Rope: He treats the frenzy as a "harness" he can apply 
 * [cite_start]or remove[cite: 107].
 * Pentheus -> Noose: He views it as a "plot" and "compact" designed 
 * [cite_start]to subvert his kingship[cite: 934, 943].
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    divine_presence,
    "Is Dionysus a literal metaphysical entity (Mountain) or a metaphor for socio-psychological mass hysteria (Noose)?",
    [cite_start]resolution_mechanism("Requires empirical verification of 'voice from heaven' [cite: 1345] vs collective hallucination."),
    impact("If Mountain: The play is about man vs law of nature. If Noose: The play is about social breakdown."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * 1. Traditional Civic Religion: Pentheus attempts to maintain the "laws 
 * [cite_start]that live"[cite: 1100], but they are overwhelmed by the new "magic."
 * * CONCLUSION: 
 * Because Pentheus' alternative (traditional law) is actively suppressed 
 * by divine miracles (shaking the house, unloosing gyves), the constraint 
 * shifts from Rope to Noose for the Theban state.
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(dionysaic_frenzy, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(dionysaic_frenzy, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(dionysaic_frenzy, noose, agent_power(individual_powerless)).
