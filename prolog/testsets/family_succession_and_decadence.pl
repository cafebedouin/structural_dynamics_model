% [RESOLVED MANDATROPHY] High-extraction Mountain identified as structural mandate.
% ============================================================================
% CONSTRAINT STORY: family_succession_and_decadence (kazoku_keishoku)
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: Jun'ichirō Tanizaki, "Atsumono" (The Hot Soup)
% ============================================================================

:- module(constraint_atsumono, []).

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
 * * constraint_id: family_succession_system
 * human_readable: Meiji-Taisho Family Succession and the "Ie" System
 * domain: social/familial/ethical
 * temporal_scope: Late Meiji - Early Taisho Era (c. 1910s)
 * spatial_scope: Japan (Tokyo, Odawara, Numazu)
 * * SUMMARY:
 * The pure love between the protagonist, Tachibana Sōichi, and Miyoko 
 * collides with the rigid "Ie" (House) system of the era. The requirement 
 * for a "muko-yōshi" (adopted son-in-law) to maintain Miyoko's family 
 * lineage functions as an immutable force (Mountain) that nullifies 
 * individual emotion, driving the youth toward a self-destructive 
 * cycle of decadence (Snare) as a failed exit strategy.
 * * KEY AGENTS:
 * - Tachibana Sōichi: A powerless individual (individual_powerless). Idealistic but lacks systemic leverage.
 * - Miyoko: A powerless individual and the direct object of the succession constraint.
 * - Sōichi's Mother (O-shina): A guardian of the system (institutional). Enforces the "reason" of the House.
 * - Yamaguchi: A guide to decadence. Offers vice as a false exit from the systemic trap.
 * * NARRATIVE ARC:
 * The constraint initially appears as a "Rope" (necessary social order) to the parents 
 * but reveals itself as a "Mountain" to the lovers. Its enforcement leads to a 
 * psychological "scald," causing the protagonist to retreat into a "Snare" of 
 * nihilistic dissipation in the red-light districts.
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(atsumono_period, 0, 10).
narrative_ontology:constraint_claim(family_succession_system, mountain).

% Base extractiveness score: 0.8
% Rationale: The family system extracts the total future and happiness of individuals 
% to ensure the abstract continuity of the "House" lineage.
domain_priors:base_extractiveness(family_succession_system, 0.8).

% Suppression score: 0.9
% Rationale: Alternatives like elopement or "love matches" result in social/economic 
% death (excommunication from the family), making them effectively invisible or impossible.
domain_priors:suppression_score(family_succession_system, 0.9).

% Enforcement requirements: Active enforcement by the matriarch/patriarch is required.
domain_priors:requires_active_enforcement(family_succession_system).

% Metrics for Executive Summary
narrative_ontology:constraint_metric(family_succession_system, extractiveness, 0.8).
narrative_ontology:constraint_metric(family_succession_system, suppression_requirement, 0.9).

% Beneficiaries and Victims
constraint_beneficiary(family_succession_system, ie_lineage). % The abstract House lineage
constraint_victim(family_succession_system, [tachibana_soichi, miyoko, sasaki]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: Sōichi and Miyoko - MOUNTAIN
   --------------------------------------------------------------------------
   WHO: individual_powerless (Young lovers with no financial independence)
   WHEN: biographical (This decides their entire lives)
   WHERE: trapped (No social path for a middle-class "love marriage" exists)
   SCOPE: national (The law and custom are uniform across Japan)
   
   WHY THIS CLASSIFICATION:
   For the youth, the "Ie" system is as unchangeable as a mountain. They cannot
   negotiate with the requirement for an heir; they can only collide with it and break.
   
   NARRATIVE EVIDENCE:
   Sōichi admits he lacks the "courage" to defy the system, treating the 
   engagement of Miyoko to a muko-yōshi as an inevitable catastrophe.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    family_succession_system,
    mountain,
    context(
        agent_power(individual_powerless),
        time_horizon(biographical),
        exit_options(trapped),
        spatial_scope(national)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: Mother (O-shina) - ROPE
   --------------------------------------------------------------------------
   WHO: institutional (Agent of the family's continuity)
   WHEN: generational (Thinking of the family's past and future)
   WHERE: arbitrage (Managing resources and reputation to survive)
   SCOPE: local (The household and its immediate social standing)
   
   WHY THIS CLASSIFICATION:
   To the mother, the system is a functional tool (Rope) to ensure stability 
   and prevent "shame." It is the "reason" (dōri) that guides the family safely.
   
   NARRATIVE EVIDENCE:
   O-shina speaks of the marriage as a matter of "reason" and "duty," viewing 
   the suppression of Sōichi's feelings as a necessary corrective measure.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    family_succession_system,
    rope,
    context(
        agent_power(institutional),
        time_horizon(generational),
        exit_options(arbitrage),
        spatial_scope(local)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: The Decadence Spiral (Yamaguchi's Vice) - NOOSE
   --------------------------------------------------------------------------
   WHO: individual_moderate (Sōichi after inheriting some autonomy/money)
   WHEN: immediate (Seeking instant gratification/oblivion)
   WHERE: constrained (Trapped by psychological trauma and addiction)
   SCOPE: local (The red-light districts)
   
   WHY THIS CLASSIFICATION:
   The "freedom" offered by the red-light district is a false exit. It extracts 
   Sōichi's health and vitality, tightening like a snare as he tries to 
   numb the "scald" of his failed love.
   
   NARRATIVE EVIDENCE:
   Sōichi's transition from an earnest student to a man who "knows the bad 
   ways," finding a cold pleasure in self-destruction.
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    decadence_spiral,
    snare,
    context(
        agent_power(individual_moderate),
        time_horizon(immediate),
        exit_options(constrained),
        spatial_scope(local)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(atsumono_tests).

test(perspectival_gap_analysis) :-
    % Demonstrate that the same system is a Mountain to the son but a Rope to the mother
    constraint_indexing:constraint_classification(family_succession_system, mountain, context(agent_power(individual_powerless), _, _, _)),
    constraint_indexing:constraint_classification(family_succession_system, rope, context(agent_power(institutional), _, _, _)).

test(extraction_threshold) :-
    % Verify the system is identified as highly extractive
    domain_priors:base_extractiveness(family_succession_system, E),
    E >= 0.8.

test(time_horizon_immutability) :-
    % In the biographical horizon for a powerless agent, it must be a mountain
    constraint_indexing:constraint_classification(family_succession_system, mountain, context(_, biographical, trapped, _)).

:- end_tests(atsumono_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * 1. EXTRACTIVENESS (0.8): The "Ie" system prioritizes the abstract 
 * concept of the "House" over actual human lives. Sōichi's father's 
 * history of "reforming" from a playboy to a merchant shows how the 
 * Rope of the system successfully tames individual Will.
 * * 2. THE METAPHOR (Atsumono): The title refers to the proverb "He who is 
 * scalded by hot soup (atsumono) blows on cold sashimi." The system's 
 * initial rejection is the "scald" that makes Sōichi fear all passion.
 * * 3. PERSPECTIVES: I highlighted the Institutional (Mother) vs. 
 * Powerless (Sōichi) gap to show how "reason" to one is "fate" to the other.
 * * 4. AMBIGUITIES: The true feelings of Miyoko are never fully revealed, 
 * creating an Omega regarding whether she was a silent victim or a 
 * compliant agent of her own house.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    miyoko_true_will,
    "To what extent did Miyoko actually possess the will to resist her parents' decision?",
    resolution_mechanism("Analysis of her letters vs. Yamaguchi's theory of 'woman's dual nature'"),
    impact("If she was a willing participant, the constraint shifts from Mountain to Rope for her."),
    confidence_without_resolution(medium)
).

omega_variable(
    parental_collusion,
    "Did Sōichi's father and mother agree from the start to sacrifice Sōichi's happiness?",
    resolution_mechanism("Investigation of the private correspondence between Sōichi's mother and Miyoko's mother"),
    impact("Determines if the constraint was a planned extraction (Snare) or a natural social friction (Mountain)."),
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: Elopement (Kakeochi)
 * Viability: Very low. Marriage without parental consent for those under 30 
 * was legally and socially ruinous in the Meiji Civil Code.
 * Suppression: Sōichi rejects it due to lack of "courage" and financial means.
 * * ALTERNATIVE 2: Academic Sublimation
 * Viability: Moderate. Like his friends Nakajima or Shimizu, Sōichi could 
 * have buried his grief in careerism/success.
 * Suppression: Sōichi finds academic success "meaningless" without Miyoko, 
 * showing the psychological failure of this alternative.
 * * CONCLUSION:
 * The active suppression of elopement and the emotional failure of sublimation 
 * solidify the succession system as a NOOSE/MOUNTAIN hybrid for the protagonist.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

/**
 * TO USE THIS FILE:
 * 1. Load: ?- [constraint_atsumono].
 * 2. Multi-perspective: ?- constraint_indexing:multi_index_report(family_succession_system).
 * 3. Run tests: ?- run_tests(atsumono_tests).
 */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */



% --- v3.1 Indexical Relativity Stubs (Fleet Repair) ---
constraint_indexing:constraint_classification(family_succession_and_decadence, mountain, agent_power(analytical)).
constraint_indexing:constraint_classification(family_succession_and_decadence, rope, agent_power(institutional)).
constraint_indexing:constraint_classification(family_succession_and_decadence, snare, agent_power(individual_powerless)).
