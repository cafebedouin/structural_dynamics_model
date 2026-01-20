% ============================================================================
% CONSTRAINT STORY: qwerty_vs_dvorak
% ============================================================================
% Generated: 2026-01-19
% Model: Gemini 2.0 Flash
% Source: Paul David (1985) / W. Brian Arthur / Path Dependence
% ============================================================================

:- module(constraint_qwerty_vs_dvorak, []).

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
 * * constraint_id: qwerty_vs_dvorak
 * human_readable: QWERTY vs. Dvorak (Technological Lock-in)
 * domain: technological/economic
 * temporal_scope: 1870s - Present
 * spatial_scope: Global (Digital Computing)
 * * SUMMARY:
 * This constraint represents "Path Dependence" and "Technological Lock-in." 
 * The QWERTY keyboard layout was designed to prevent mechanical jams in 19th-century 
 * typewriters. Despite the development of more efficient layouts (like Dvorak), 
 * the massive network effects and sunk costs in human training have made 
 * QWERTY an immovable standard in the digital age.
 * * KEY AGENTS:
 * - The Economic Historian: Analytical observer of path-dependent systems.
 * - The Software/Hardware Manufacturer: Institutional agent maintaining the standard for interoperability.
 * - The Dvorak Enthusiast: Individual moderate attempting to arbitrage between systems for efficiency.
 * - The Average Typist: Individual powerless subject to the historical "accident" of the layout.
 * * NARRATIVE ARC:
 * What began as a mechanical "Rope" (to solve jams) became a "Mountain" of 
 * institutional reality. For the typist, it is a "Noose"—an invisible 
 * inefficiency that strangles speed and health (RSI), yet is too costly to escape.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

% Structural Anchor
narrative_ontology:interval(qwerty_lockin_interval, 0, 10).
narrative_ontology:constraint_claim(qwerty_vs_dvorak, noose).

% Base extractiveness score (0.0-1.0)
% Rationale: Moderate (0.4). It extracts "human efficiency" and "physical health." 
% The sub-optimal layout imposes a "tax" of slower typing and higher strain 
% on billions of hands to maintain the network effect.
domain_priors:base_extractiveness(qwerty_vs_dvorak, 0.4).

% Suppression score (0.0-1.0)
% Rationale: High (0.7). Alternatives like Dvorak or Colemak are technically 
% available but socially suppressed. The cost of re-learning (sunk cost) 
% makes them conceptually invisible to the majority.
domain_priors:suppression_score(qwerty_vs_dvorak, 0.7).

% Enforcement requirements
% Emerges naturally through network effects and positive feedback loops.
domain_priors:emerges_naturally(qwerty_vs_dvorak).

% Metrics required for Section 1 of the Executive Summary
narrative_ontology:constraint_metric(qwerty_vs_dvorak, extractiveness, 0.4).
narrative_ontology:constraint_metric(qwerty_vs_dvorak, suppression_requirement, 0.7).

% BENEFICIARIES & VICTIMS
constraint_beneficiary(qwerty_vs_dvorak, [legacy_manufacturers, educational_institutions]).
constraint_victim(qwerty_vs_dvorak, [high_speed_typists, rsi_sufferers]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE ECONOMIC HISTORIAN - Mountain
   --------------------------------------------------------------------------
   
   WHO: analytical - Observer of historical trajectories and path dependence.
   WHEN: civilizational - Viewing the standard as a permanent feature of the era.
   WHERE: analytical - Not personally constrained, but mapping the "wall."
   SCOPE: global - The standard spans the entire digital world.
   
   WHY THIS CLASSIFICATION:
   To the historian, the QWERTY standard is a Mountain. It is an unchangeable 
   feature of the technological landscape created by "quasi-irreversibility." 
   Once the network effect crossed the threshold, the chance of shifting 
   dropped to zero; it is a fixed peak of history.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    qwerty_vs_dvorak,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(civilizational),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE TECH GIANT (Apple/Microsoft) - Rope
   --------------------------------------------------------------------------
   
   WHO: institutional - Power to define hardware and software defaults.
   WHEN: biographical - Maintaining market dominance over 30-50 years.
   WHERE: arbitrage - Can offer Dvorak as an option but keeps QWERTY as default.
   SCOPE: global - Global OS distribution.
   
   WHY THIS CLASSIFICATION:
   For the institution, QWERTY is a Rope. It is the ultimate coordination 
   mechanism. It ensures that every computer sold is immediately usable by 
   every customer. They use the constraint to "tether" the world to their 
   products, ensuring interoperability and minimizing support costs.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    qwerty_vs_dvorak,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(arbitrage),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE OFFICE WORKER - Noose
   --------------------------------------------------------------------------
   
   WHO: individual_powerless - Subject to the hardware provided by the employer.
   WHEN: immediate - Today's 8-hour typing shift.
   WHERE: trapped - Cannot change the hardware or re-train without loss of income.
   SCOPE: local - Immediate desk/workstation.
   
   WHY THIS CLASSIFICATION:
   For the worker, QWERTY is a Noose. They must use a layout designed to 
   slow them down (initially to prevent jams) which now causes physical 
   strain and limits their output. The "network effect" strangles their 
   individual choice, as using an alternative makes them "incompatible" 
   with other people's computers and shared spaces.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    qwerty_vs_dvorak,
    noose,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(qwerty_lockin_tests).

test(multi_perspective_variance) :-
    % Analyst sees Mountain, Institutional sees Rope, Powerless sees Noose
    constraint_indexing:constraint_classification(qwerty_vs_dvorak, mountain, context(analytical, civilizational, analytical, global)),
    constraint_indexing:constraint_classification(qwerty_vs_dvorak, rope, context(institutional, biographical, arbitrage, global)),
    constraint_indexing:constraint_classification(qwerty_vs_dvorak, noose, context(individual_powerless, immediate, trapped, local)).

test(power_extractiveness_efficiency) :-
    % Powerless individuals feel the extraction of their time/health (Noose).
    % Institutions use the standard to manage global supply chains (Rope).
    ContextPowerless = context(individual_powerless, immediate, trapped, local),
    ContextPowerful = context(institutional, biographical, arbitrage, global),
    constraint_indexing:extractiveness_for_agent(qwerty_vs_dvorak, ContextPowerless, Score1),
    constraint_indexing:extractiveness_for_agent(qwerty_vs_dvorak, ContextPowerful, Score2),
    Score1 > Score2.

test(time_immutability_lockin) :-
    % Short-term trapped = Mountain/Noose.
    constraint_indexing:effective_immutability(biographical, trapped, mountain).

:- end_tests(qwerty_lockin_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * Date: 2026-01-19
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.4): QWERTY "extracts" human ergonomics. It is a 
 * legacy of mechanical friction that now taxes digital human output.
 * * 2. SUPPRESSION (0.7): High. Not because Dvorak is banned, but because 
 * "Switching Costs" are so high that they act as a natural silencer for 
 * any alternative.
 * * 3. PERSPECTIVES: Chose the Historian (Law), the Corp (Tool), and the 
 * Worker (Victim) to illustrate the perspectival gap.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    llm_input_decoupling,
    "Will AI/Voice/Neural interfaces untie the QWERTY Noose (Rope), or will the 
    legacy layout merely persist as a symbolic 'Mountain' of human interaction?",
    resolution_mechanism("Monitor the adoption of non-textual input methods in professional settings"),
    impact("If Rope: Inefficiency is finally escaped. If Mountain: The layout 
    remains as a cultural artifact/Noose even when the original need is gone."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Dvorak Simplified Keyboard
 * Viability: High. Scientifically proven to reduce finger travel and 
 * increase comfort.
 * Suppression: High. Sunk costs in training and hardware manufacturing.
 * * ALTERNATIVE 2: Colemak / Workman
 * Viability: Modern layouts optimized for computers, keeping shortcuts (Z,X,C,V) 
 * in place while moving common letters to the home row.
 * * CONCLUSION:
 * The existence of superior alternatives (Dvorak/Colemak) that are effectively 
 * unusable due to network effects proves that QWERTY is a Noose for the 
 * individual, despite being a Rope for global commerce.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * ?- [constraint_qwerty_vs_dvorak].
 * ?- multi_index_report(qwerty_vs_dvorak).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
