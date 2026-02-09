% ============================================================================
% CONSTRAINT STORY: magna_carta_liberties
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: The Magna Carta (1215)
% ============================================================================

:- module(constraint_magna_carta_liberties, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile 
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: magna_carta_liberties
 * human_readable: The Great Charter of Liberties
 * domain: political/legal
 * temporal_scope: 1215 CE (High Middle Ages)
 * spatial_scope: Kingdom of England
 * * SUMMARY:
 * The Magna Carta is a peace treaty between King John and a group of rebellious 
 * barons. It establishes that the monarch is not above the law and defines specific 
 * liberties regarding due process (Clause 39), church freedom, and limits on 
 * feudal payments ("reliefs"). It introduces a security clause (Clause 61) 
 * empowering a council of twenty-five barons to "distrain and distress" the King 
 * if he fails to uphold the charter.
 * * KEY AGENTS:
 * - King John: The institutional ruler whose absolute power is being curtailed.
 * - The Twenty-Five Barons: The collective organized enforcers of the security clause.
 * - The Free Man: The individual subject who gains specific protections against arbitrary seizure.
 * * NARRATIVE ARC:
 * Born from "discord that has arisen," the Charter moves from absolute royal 
 * prerogative toward a coordinated legal order. It seeks to "allay the discord" 
 * by turning the King's arbitrary power into a "Rope" of shared rules, though 
 * to the King, it is experienced as a "Snare" of coercion.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(magna_carta_liberties, 0, 10).
narrative_ontology:constraint_claim(magna_carta_liberties, rope).

% Base extractiveness: Moderate (0.4)
% Rationale: While it limits the King's extraction, it codifies standard 
% feudal "reliefs" (100 pounds for a barony) and services[cite: 2, 16].
domain_priors:base_extractiveness(magna_carta_liberties, 0.4).

% Suppression score: High (0.8)
% Rationale: Explicitly abolishes "evil customs" regarding forests and 
% mandates the removal of foreign mercenaries[cite: 48, 51].
domain_priors:suppression_score(magna_carta_liberties, 0.8).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(magna_carta_liberties, extractiveness, 0.4).
narrative_ontology:constraint_metric(magna_carta_liberties, suppression_requirement, 0.8).

% Enforcement: Requires active monitoring by the 25 barons.
domain_priors:requires_active_enforcement(magna_carta_liberties).

% Metrics for Section 1 of the Executive Summary
% BENEFICIARIES & VICTIMS
narrative_ontology:constraint_beneficiary(magna_carta_liberties, [the_barons, the_english_church, free_men]).
narrative_ontology:constraint_victim(magna_carta_liberties, [arbitrary_monarchy, foreign_mercenaries]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE FREE MAN - Mountain
   --------------------------------------------------------------------------
   
   WHO: powerless - Subject to "the law of the land".
   WHEN: immediate - Experience of arrest or seizure.
   WHERE: trapped - Bound by the jurisdiction of the county courts[cite: 18].
   SCOPE: national - Applies "throughout our whole realm"[cite: 35].
   
   WHY THIS CLASSIFICATION:
   For the individual free man, the Charter's protections (like standard 
   weights and measures or due process) function as an unchangeable 
   "Mountain." He does not have the power to negotiate these rules; 
   they are presented as "confirmed for us and our heirs in perpetuity" 
   [cite: 1].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    magna_carta_liberties,
    mountain,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        constraint_beneficiary(magna_carta_liberties, free_men),
        constraint_victim(magna_carta_liberties, []),
        spatial_scope(national)
    )
) :-
    domain_priors:suppression_score(magna_carta_liberties, S),
    S > 0.7,
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE BARONS - Rope
   --------------------------------------------------------------------------
   
   WHO: collective_organized - The twenty-five barons elected to "keep the peace".
   WHEN: biographical - During their tenure as guardians.
   WHERE: arbitrage - They balance the King's needs against the Charter's violations.
   SCOPE: national - Protecting the "whole community of the land".
   
   WHY THIS CLASSIFICATION:
   To the Barons, the Charter is a "Rope"—a functional coordination mechanism. 
   It provides the specific procedures ("forty days notice," "majority verdict") 
   by which they can manage the King's behavior and their own cooperation 
   [cite: 14, 61].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    magna_carta_liberties,
    rope,
    context(
        agent_power(collective_organized),
        time_horizon(biographical),
        exit_options(arbitrage),
        constraint_beneficiary(magna_carta_liberties, the_barons),
        constraint_victim(magna_carta_liberties, []),
        spatial_scope(national)
    )
) :-
    !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: KING JOHN - Snare
   --------------------------------------------------------------------------
   
   WHO: institutional - The monarch whose will is restricted.
   WHEN: immediate - Under the threat of distraint by the barons.
   WHERE: constrained - Prevented from seizing wood or carts without consent[cite: 30, 31].
   SCOPE: national - His entire executive power is limited.
   
   WHY THIS CLASSIFICATION:
   For King John, the Charter is a "Snare." It is a coercive mechanism 
   imposed by the Barons that extracts his absolute authority and 
   authorizes the community to "assail us in every way possible" if 
   he deviates from the text.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    magna_carta_liberties,
    snare,
    context(
        agent_power(institutional),
        time_horizon(immediate),
        exit_options(constrained),
        constraint_beneficiary(magna_carta_liberties, the_barons),
        constraint_victim(magna_carta_liberties, arbitrary_monarchy),
        spatial_scope(national)
    )
) :-
    domain_priors:base_extractiveness(magna_carta_liberties, E),
    E > 0.3,
    !.

/* ==========================================================================
   4. TESTS (Validation of Insights)
   ========================================================================== */

:- begin_tests(magna_carta_tests).

test(baronial_coordination) :-
    % Barons see a Rope for coordination
    constraint_indexing:constraint_classification(magna_carta_liberties, rope, context(agent_power(collective_organized), _, _, _, _, _)).

test(monarch_coercion) :-
    % King sees a Snare because of Clause 61 (distraint)
    constraint_indexing:constraint_classification(magna_carta_liberties, snare, context(agent_power(institutional), _, constrained, _, _, _)).

test(due_process_mountain) :-
    % For the powerless, due process is a fixed Mountain of protection
    constraint_indexing:constraint_classification(magna_carta_liberties, mountain, context(agent_power(powerless), _, _, _, _, _)).

:- end_tests(magna_carta_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * GENERATING MODEL INTERPRETATION:
 * * The Magna Carta presents a classic "Perspectival Gap." 
 * 1. The document is ostensibly a "Rope" for the kingdom, intending to 
 * provide "better ordering"[cite: 1]. 
 * 2. However, Clause 61 (the Security Clause) is the pivot. By authorizing 
 * the barons to seize royal property, it moves the King from a "Rope" 
 * maker to a "Snare" subject. 
 * 3. Interestingly, the document attempts to frame its own "Mountain" 
 * nature by using the term "in perpetuity"[cite: 1], signaling that 
 * these rights are now part of the natural law of England.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    clause_61_viability,
    "Could a council of 25 barons effectively coordinate a seizure of royal assets without devolving into civil war?",
    resolution_mechanism("Historical record of the First Barons' War immediately following 1215."),
    impact("If civil war is inevitable, Clause 61 is a failed coordination mechanism (slack Rope)."),
    confidence_without_resolution(low)
).

omega_variable(
    free_man_definition,
    "To what extent did the 'liberties' apply to unfree villeins vs. landed elite?",
    resolution_mechanism("Analysis of Clause 20 vs. Clause 39 language regarding 'villeins' and 'free men'."),
    impact("If exclusive, the 'Mountain' of rights exists only for the elite, leaving the poor in a Snare of local lords."),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES:
 * * ALTERNATIVE 1: Absolute Monarchy (Angevin Kingship)
 * Viability: The status quo established by Henry II and Richard I.
 * Suppression: Rejected by the barons through military rebellion.
 * Evidence: The Preamble notes the "discord that has arisen"[cite: 1].
 * * ALTERNATIVE 2: Papal Supremacy
 * Viability: King John had already made England a papal fief in 1213.
 * Suppression: The Barons ignored the Pope's later annulment of the charter.
 * Evidence: Clause 1 mentions ratification by Pope Innocent III[cite: 1].
 * * CONCLUSION:
 * The rejection of Absolute Monarchy (Alternative 1) is what necessitates 
 * the "Snare" classification from the King's perspective. He is no 
 * longer the source of law but its subject.
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
% Coordination mechanism in political domain — moderate institutional framing
domain_priors:theater_ratio(magna_carta_liberties, 0.17).
narrative_ontology:constraint_metric(magna_carta_liberties, theater_ratio, 0.17).

% --- Analytical perspective classification (missing) ---
% chi = 0.4 * 1.15 (analytical) * 1.2 (global) = 0.552
% Classification: tangled_rope
constraint_indexing:constraint_classification(magna_carta_liberties, tangled_rope,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).
