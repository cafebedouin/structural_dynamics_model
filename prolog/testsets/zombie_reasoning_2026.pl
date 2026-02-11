% ============================================================================
% CONSTRAINT STORY: zombie_reasoning_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-06
% ============================================================================

:- module(constraint_zombie_reasoning_2026, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Namespace Hooks (Required for loading) ---
:- multifile
    domain_priors:base_extractiveness/2,
    domain_priors:suppression_score/2,
    domain_priors:theater_ratio/2,
    domain_priors:requires_active_enforcement/1,
    narrative_ontology:has_sunset_clause/1,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    constraint_indexing:constraint_classification/3.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: zombie_reasoning_2026
 * human_readable: The Zombie Reasoning Epistemic Snare
 * domain: philosophical/technological
 * * SUMMARY:
 * Rebecca Lowe argues that current AI discourse is plagued by the loose use of 
 * terms like "reasoning," "evaluating," and "selecting". 
 * Because AI lacks the interiority and phenomenological consciousness 
 * necessary for true reasoning, these activities are classified as 
 * "zombie reasoning"—shadow versions that mimic the outputs of a self-aware 
 * agent without possessing an inner life.
 * * KEY AGENTS:
 * - Human Reasoning Subjects: Subject (Powerless against anthropomorphism)
 * - AI Industry/Glossary Authors (Nvidia): Beneficiary (Institutional)
 * - Philosophers of Liberalism (Rebecca Lowe): Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is high (0.74). Anthropomorphizing AI extracts the unique 
% moral and normative status of human self-awareness.
domain_priors:base_extractiveness(zombie_reasoning_2026, 0.74). 

% Suppression is high (0.80). The ubiquitous use of "reasoning" in AI 
% products suppresses the distinction between living and non-living 
% entities.
domain_priors:suppression_score(zombie_reasoning_2026, 0.80).   

% Theater ratio is extreme (0.88). AI "actions" are identified as a 
% performative mask over automated signal ordering.
domain_priors:theater_ratio(zombie_reasoning_2026, 0.88).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(zombie_reasoning_2026, extractiveness, 0.74).
narrative_ontology:constraint_metric(zombie_reasoning_2026, suppression_requirement, 0.8).
narrative_ontology:constraint_metric(zombie_reasoning_2026, theater_ratio, 0.88).

% Constraint classification claim
narrative_ontology:constraint_claim(zombie_reasoning_2026, snare).

% Primary keys for the classification engine
% High-extraction stakeholders
narrative_ontology:constraint_beneficiary(zombie_reasoning_2026, generative_ai_marketing).
narrative_ontology:constraint_victim(zombie_reasoning_2026, human_epistemic_clarity).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE HUMAN AGENT (SNARE)
% For the human user, loose language is a Snare: a "slippery" trap that 
% deadens awareness of truths and leaves one open to manipulation 
% .
constraint_indexing:constraint_classification(zombie_reasoning_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(national))).

% PERSPECTIVE 2: THE AI INDUSTRY (ROPE)
% Companies view "AI Reasoning" as a Rope: an essential coordination 
% narrative to explain how systems solve problems and interact with 
% users.
constraint_indexing:constraint_classification(zombie_reasoning_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SNARE)
% Analysts see anthropomorphic language as a Snare: high extraction of
% epistemic clarity (ε=0.74), extreme theater (0.88), and high suppression
% (0.80) — the performative mask of "reasoning" actively deadens
% awareness of the consciousness gap.
constraint_indexing:constraint_classification(zombie_reasoning_2026, snare,
    context(agent_power(analytical),
            time_horizon(civilizational),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(zombie_reasoning_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(zombie_reasoning_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(zombie_reasoning_2026, rope, context(agent_power(institutional), _, _, _)).

test(theater_check) :-
    domain_priors:theater_ratio(zombie_reasoning_2026, TR),
    TR >= 0.70.

:- end_tests(zombie_reasoning_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.74) is justified by the moral risk: reasoning is 
 * what makes humans candidates for blameworthiness. 
 * If AI "reasons" without self-awareness, it is merely "zombie reasoning"—a 
 * shadow version that lacks normative force.
 *
 * * MANDATROPHY ANALYSIS:
 * [RESOLVED MANDATROPHY]
 * The system identifies the Snare by the epistemic risk of anthropomorphism. 
 * Loose talk about "happy Moltbots" or AI "thinking" obscures the 
 * fundamental lack of interiority in non-living systems.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_interiority_verification,
    'Can interiority be functionally proven without relying on private phenomenology?',
    'Analysis of public criteria for reasoning vs. inner life.',
    'Success converts the Mountain to a Rope; Failure maintains the Snare.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(zombie_reasoning_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Modeling the drift from "Ordinary Language" to "Zombie Vernacular."
% Theater ratio rises as AI outputs increasingly mimic human deliberation.
narrative_ontology:measurement(zr_tr_t0, zombie_reasoning_2026, theater_ratio, 0, 0.30).
narrative_ontology:measurement(zr_tr_t5, zombie_reasoning_2026, theater_ratio, 5, 0.65).
narrative_ontology:measurement(zr_tr_t10, zombie_reasoning_2026, theater_ratio, 10, 0.88).

% Extraction rises as anthropomorphism deadens human awareness of AI realities.
narrative_ontology:measurement(zr_ex_t0, zombie_reasoning_2026, base_extractiveness, 0, 0.20).
narrative_ontology:measurement(zr_ex_t5, zombie_reasoning_2026, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(zr_ex_t10, zombie_reasoning_2026, base_extractiveness, 10, 0.74).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
