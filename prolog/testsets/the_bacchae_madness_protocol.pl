% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: the_bacchae_madness_protocol
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: The Bacchae of Euripides (Gilbert Murray Translation)
% ============================================================================

:- module(constraint_the_bacchae, []).

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
 * * constraint_id: the_bacchae_madness_protocol
 * human_readable: The Dionysian Mandate of Ecstasy
 * domain: religious/political/social
 * temporal_scope: Ancient Greek (Mythological)
 * spatial_scope: Thebes / Mount Cithaeron
 * * SUMMARY:
 * This constraint models the tension between Pentheus' "Mandate of Order" and 
 * the Dionysian "Mandate of Ecstasy." Pentheus attempts to treat the rising 
 * cult of Dionysus as a criminal deviation to be suppressed by law (Noose). 
 * However, Dionysus operates as a Mountain—an unyielding psychological and 
 * divine force that extracts the sanity and life of the King when he refuses 
 * to coordinate with the new reality.
 * * KEY AGENTS:
 * - Pentheus: Institutional; King of Thebes who prioritizes a rigid "Order" 
 * and views the margin of religious ecstasy as a social threat.
 * - Dionysus (The Stranger): Individual Powerful/Divine; the architect of 
 * the "Ecstasy Mandate" who extracts Pentheus' dignity and life.
 * - Agave: Individual Powerless; caught in the "Bacchic Noose," she becomes 
 * the instrument of the mandate's terminal extraction.
 * * NARRATIVE ARC:
 * Pentheus views his laws as a Rope (coordination for a stable city). 
 * To the Bacchants, the god is a Rope (communal liberation). However, 
 * as Pentheus tightens his "Order," Dionysus turns the King's own 
 * curiosity into a Noose, leading him to Mount Cithaeron (The Mountain) 
 * where the "Mandatrophy" of Pentheus' rigid psyche results in his total 
 * biological and political destruction.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor for index extraction
narrative_ontology:interval(theban_dionysian_conflict, 0, 10).
narrative_ontology:constraint_claim(the_bacchae_madness_protocol, noose).

% Base extractiveness score (0.0-1.0)
% Rationale: 0.95. Dionysus extracts everything—Pentheus’ status, his 
% masculinity, his sanity, and finally his physical body—to satisfy the 
% mandate of divine recognition.
domain_priors:base_extractiveness(the_bacchae_madness_protocol, 0.95).

% Suppression score (0.0-1.0)
% Rationale: 0.85. Pentheus suppresses the women's autonomy; Dionysus 
% suppresses Pentheus' rationality and the city's established laws 
%.
domain_priors:suppression_score(the_bacchae_madness_protocol, 0.85).

% Enforcement requirements
% Requires active enforcement (Dionysus' spells, Pentheus' guards, 
% and the frenzied violence of the Maenads).
domain_priors:requires_active_enforcement(the_bacchae_madness_protocol).

% Metrics for Executive Summary
narrative_ontology:constraint_metric(the_bacchae_madness_protocol, extractiveness, 0.95).
narrative_ontology:constraint_metric(the_bacchae_madness_protocol, suppression_requirement, 0.85).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(the_bacchae_madness_protocol, dionysian_divine_authority).
constraint_beneficiary(the_bacchae_madness_protocol, religious_pluralism_via_disruption).
constraint_victim(the_bacchae_madness_protocol, pentheus).
constraint_victim(the_bacchae_madness_protocol, theban_civic_stability).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: PENTHEUS - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to command the army and imprison "the stranger" 
        .
   WHEN: immediate - Focused on the "scandal" in the hills.
   WHERE: mobile - Moving between the palace and the mountain.
   SCOPE: national - Protecting the "decency" of Thebes.
   
   WHY THIS CLASSIFICATION:
   Pentheus views his military and legal actions as a Rope. He believes he is 
   weaving a net to catch a disruptive criminal and re-coordinate his city 
   under the light of "Reason".
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    the_bacchae_madness_protocol,
    rope,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(the_bacchae_madness_protocol, E),
    E < 0.99,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE MAENADS / AGAVE - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Caught in a "divine net" they cannot resist 
        .
   WHEN: immediate - Tactical frenzy; "the spirit has taken them."
   WHERE: trapped - On Mount Cithaeron, away from the domestic home.
   SCOPE: local - Their immediate physical experience of ecstasy/violence.
   
   WHY THIS CLASSIFICATION:
   For the women, the madness is a Noose. It extracts their memory and 
   maternal instinct, "choking" their previous social identity to turn 
   them into hunters for the god. Agave only realizes the Noose has 
   closed when she sees her son's head in her hands.
   -------------------------------------------------------------------------- */



constraint_indexing:constraint_classification(
    the_bacchae_madness_protocol,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :-
    domain_priors:requires_active_enforcement(the_bacchae_madness_protocol),
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: CADMUS / TIRESIAS - Mountain
   --------------------------------------------------------------------------
   
   WHO: individual_moderate/analytical - Elders who recognize the unyielding 
         power of the new god.
   WHEN: historical - Looking at the survival of the bloodline.
   WHERE: mobile - They go to the mountain "clothed in fawn-skin."
   SCOPE: national - The fate of the house of Cadmus.
   
   WHY THIS CLASSIFICATION:
   The elders recognize Dionysus as a Mountain. His power is an unchangeable 
   law of the world. They do not try to "untie" the god; they simply 
   coordinate with him to avoid being crushed. To them, Pentheus' resistance 
   is a futile attempt to fight gravity.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    the_bacchae_madness_protocol,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(mobile),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(the_bacchae_madness_protocol, S),
    S > 0.5,
    !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(the_bacchae_tests).

test(ecstasy_vs_order_variance) :-
    % King (Rope) vs Elders (Mountain)
    constraint_indexing:constraint_classification(the_bacchae_madness_protocol, T1, context(institutional, immediate, mobile, national)),
    constraint_indexing:constraint_classification(the_bacchae_madness_protocol, T2, context(analytical, historical, mobile, national)),
    T1 \= T2.

test(terminal_extraction_signature) :-
    % Dionysian possession extracts the entire life-margin of Pentheus.
    domain_priors:base_extractiveness(the_bacchae_madness_protocol, E),
    E > 0.9.

:- end_tests(the_bacchae_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. MANDATROPHY ANALYSIS: Pentheus' "Mandate of Order" atrophied his city's 
 * ability to metabolize spiritual "disruption" (The Margin). By extracting 
 * religious tolerance to satisfy his ego/order, he "choked" his own ability 
 * to see the god standing before him.
 * 2. CLASSIFICATION: I primary-labeled it as a 'Noose' because the god’s 
 * arrival is an extractive event that demands total psychological and 
 * political submission.
 * 3. PERSPECTIVE: The "Analytical" view of the elders shows that 
 * acknowledging a Mountain (divinity) is the only way to avoid the Noose.
 */

omega_variable(
    divine_intervention_necessity,
    "Was Pentheus' death a result of his own 'Mandatrophy' (internal 
     atrophy of margin) or an external 'Noose' applied by a vengeful god?",
    resolution_mechanism("Analysis of Pentheus' options for coordination 
    prior to his cross-dressing transformation"),
    impact("If he had viable exits: The god is a Rope. If he was trapped 
            from the start: The god is a Noose."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   6. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Diplomatic Integration (Cadmus' Way)
 * Viability: Cadmus and Tiresias prove that "joining the dance" saves 
 * the individual from the god's violence.
 * Suppression: Actively suppressed by Pentheus' "Mandate of Reason," 
 * which views ritual as "shameful" and "irrational".
 * * ALTERNATIVE 2: Pure Repression (The Failed Noose)
 * Viability: Pentheus' attempt to jail the god.
 * Suppression: Fails because the god's power is a "Mountain"—physical 
 * chains simply fall off.
 * * CONCLUSION:
 * The existence of Alternative 1 (Coordination) which was rejected for 
 * Alternative 2 (Repression) proves that Pentheus' tragedy is a result 
 * of choosing a Noose over a Rope.
 */

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% 1. Load: ?- [the_bacchae_madness_protocol].
% 2. Analyze: ?- multi_index_report(the_bacchae_madness_protocol).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
