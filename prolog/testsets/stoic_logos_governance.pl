% ============================================================================
% CONSTRAINT STORY: stoic_logos_governance (marcus_aurelius_meditations)
% ============================================================================
% Generated: 2024-05-24
% Model: Gemini 2.0 Flash
% Source: Marcus Aurelius, "Meditations" (Long/Casaubon Translation)
% ============================================================================

:- module(constraint_stoic_governance, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks ---
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
 * * constraint_id: stoic_logos_framework
 * human_readable: The Stoic Moral and Imperial Framework
 * domain: philosophical/political/ethical
 * temporal_scope: 161–180 AD (Roman Empire)
 * spatial_scope: Global (Roman Empire and the Cosmos)
 * * SUMMARY:
 * The "Meditations" represent a unique case where the highest institutional 
 * power (the Emperor) voluntarily submits himself to a metaphysical 
 * constraint (the Logos/Nature). This constraint functions as a "Mountain" 
 * regarding the inevitability of death and fate, a "Rope" for personal 
 * discipline, and a "Noose" for the extraction of individual happiness 
 * in favor of imperial duty.
 * * KEY AGENTS:
 * - Marcus Aurelius: The "Institutional" agent who views his power as a 
 * servitude to the "Common Good."
 * - The Logos (Universal Nature): The "Powerful" metaphysical agent that 
 * sets the rules of the game.
 * - The Roman Subjects: The "Powerless" collective whose welfare is the 
 * justification for the Emperor's self-suppression.
 * - The Christians: An "Out-group" viewed through the lens of "Noose" 
 * (systemic suppression) despite the Emperor's personal gentleness.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION
   ========================================================================== */

narrative_ontology:interval(antonine_period, 0, 10).
narrative_ontology:constraint_claim(stoic_logos_framework, mountain).

% Base extractiveness: 0.7
% Rationale: The system extracts personal desire, family mourning, and 
% individual comfort to maintain the "health of the whole" (the City of God).
domain_priors:base_extractiveness(stoic_logos_framework, 0.7).

% Suppression score: 0.8
% Rationale: Active suppression of "fancies," "passions," and "anger" is 
% the core requirement for participation in the Stoic life.
domain_priors:suppression_score(stoic_logos_framework, 0.8).

% Requires Active Enforcement: Yes (Internalized through constant meditation).
domain_priors:requires_active_enforcement(stoic_logos_framework).

% Metrics
narrative_ontology:constraint_metric(stoic_logos_framework, extractiveness, 0.7).
narrative_ontology:constraint_metric(stoic_logos_framework, suppression_requirement, 0.8).

% Beneficiaries: The Universal Community (Cosmopolis), the Roman State.
constraint_beneficiary(stoic_logos_framework, cosmopolis).
% Victims: Marcus's personal autonomy, his leisure, and historical out-groups (Christians).
constraint_victim(stoic_logos_framework, [marcus_personal_will, christian_sectaries]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS
   ========================================================================== */

/* --------------------------------------------------------------------------
   Perspective 1: Marcus as Ruler - NOOSE
   --------------------------------------------------------------------------
   To the Emperor, the duty of Rome is a "Noose." It extracts his vitality 
   and forces him to live "in the court" while longing for the "mother" 
   (Philosophy). He is trapped by his rank.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    stoic_logos_framework,
    noose,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   Perspective 2: The Individual vs. Mortality - MOUNTAIN
   --------------------------------------------------------------------------
   Death and Change are presented as "Natural Laws." There is no arbitrage; 
   one must simply accept. This is the "Mountain" that cannot be moved.
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    stoic_logos_framework,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   Perspective 3: Philosophy as a Tool - ROPE
   --------------------------------------------------------------------------
   When Marcus uses "Dogmata" to calm his mind, philosophy acts as a "Rope." 
   It is a coordination mechanism between his reason and his actions, 
   allowing for internal arbitrage (peace in a palace).
   -------------------------------------------------------------------------- */
constraint_indexing:constraint_classification(
    stoic_logos_framework,
    rope,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(arbitrage),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TEST SUITE
   ========================================================================== */

:- begin_tests(stoic_tests).

test(duty_as_extraction) :-
    domain_priors:base_extractiveness(stoic_logos_framework, E), E > 0.5.

test(perspectival_shift) :-
    % A ruler sees duty as a Noose; a student sees philosophy as a Rope.
    constraint_indexing:constraint_classification(stoic_logos_framework, noose, context(agent_power(institutional), _, _, _)),
    constraint_indexing:constraint_classification(stoic_logos_framework, rope, context(agent_power(individual_moderate), _, _, _)).

:- end_tests(stoic_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION
   ========================================================================== */
/**
 * INTERPRETATION:
 * The "Meditations" illustrate the ultimate paradox of Deferential Realism: 
 * the person with the most power in the world (the Princeps) classifies 
 * himself as "individual_powerless" in the face of the Logos.
 *
 * ANALYSIS POINTS:
 * 1. THE ANTONINE PLAGUE: Marcus mentions his constitution is weak and many 
 * are dying. Statistics suggest the Antonine Plague (165–180 AD) killed 
 * approximately 5 to 10 million people (roughly 10% of the Roman population).
 * 2. VOLUNTARY EXTRACTION: Unlike typical "Nooses" where power extracts 
 * from the powerless, Marcus creates a "Noose" for himself to ensure the 
 * "Rope" of social order remains for his subjects.
 * 3. THE CHRISTIAN ANOMALY: Marcus's dismissal of Christians as 
 * "passionately set upon opposition" shows a failure to apply the 
 * "Rope" (logic) to an out-group, treating them instead with the 
 * unyielding force of the "Mountain" (state law).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω)
   ========================================================================== */

omega_variable(internal_sincerity, 
    "To what extent was Marcus's self-debasement a performance for his own ego?", 
    "The privacy of the text suggests high sincerity, but the 'Stoic Pose' is a known social construct.").

omega_variable(christian_persecution_intent, 
    "Was the persecution of Christians a direct result of Stoic dogma or administrative inertia?", 
    "The text suggests he viewed them as irrational, which in Stoicism justifies suppression.").

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */
/**
 * ALTERNATIVE 1: Epicureanism (Ataraxia)
 * Viability: Low for an Emperor. Epicureanism's "Live in Hiding" is 
 * impossible for a ruler.
 * Suppression: Marcus rejects it because it lacks the "Social" component 
 * necessary for a Roman.
 *
 * ALTERNATIVE 2: Tyranny (Nero/Commodus Path)
 * Viability: High. Commodus (his son) took this path.
 * Suppression: Marcus views this as a "Noose" for the soul—it extracts 
 * one's humanity for the sake of temporary pleasure.
 */

/* ==========================================================================
   END OF EVALUATION
   ========================================================================== */


% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(stoic_logos_governance, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(stoic_logos_governance, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(stoic_logos_governance, noose, agent_power(individual_powerless)).
