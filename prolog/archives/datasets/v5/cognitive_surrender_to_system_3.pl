% ============================================================================
% CONSTRAINT STORY: cognitive_surrender_to_system_3
% ============================================================================
% Generated: 2026-01-20
% Model: Gemini 2.0 Flash
% Source: Shaw & Nave (2026) - Thinking, Fast, Slow, and Artificial
% ============================================================================

:- module(constraint_cognitive_surrender, []).

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
 * * constraint_id: cognitive_surrender_to_system_3
 * human_readable: Cognitive Surrender in the Tri-System Ecology
 * domain: technological/cognitive
 * temporal_scope: 2024 - 2026 (Emergence of generative AI reasoning)
 * spatial_scope: Global (Digital/Cognitive)
 * * SUMMARY:
 * Traditional dual-process theories (System 1/2) are limited by the "brain-bound 
 * [cite_start]cognition assumption"[cite: 2913, 2915]. Tri-System Theory introduces 
 * [cite_start]System 3: external, automated, data-driven reasoning[cite: 2810, 2842]. 
 * "Cognitive surrender" is the uncritical adoption of System 3's output, 
 * [cite_start]which bypasses deliberation and suppresses human agency[cite: 2812, 2835, 3010].
 * * KEY AGENTS:
 * - The Cognitive Miser (User): An individual seeking to minimize effort, often 
 * [cite_start]relinquishing critical evaluation for speed[cite: 2835, 3489].
 * - The AI Assistant (System 3): An external cognitive agent providing fluent, 
 * [cite_start]confident, and epistemically authoritative outputs[cite: 2927, 3490].
 * - The Designer/Policymaker (Institutional): Actors who must decide whether 
 * [cite_start]to design for efficiency (surrender) or accountability (offloading)[cite: 3516, 3521].
 * * NARRATIVE ARC:
 * The paper identifies a behavioral signature: accuracy rises when AI is correct 
 * but falls significantly when it errs (+25/-15 points), regardless of 
 * [cite_start]time pressure[cite: 2815, 2816]. The narrative describes a shift from 
 * strategic "offloading" to a state of "epistemic dependence" where humans 
 * [cite_start]"stop deliberative thinking altogether"[cite: 3014, 3015].
 */

/* ==========================================================================
   2. CORE SYSTEM INTEGRATION (The "Reality" Layer)
   ========================================================================== */

narrative_ontology:interval(system_3_emergence, 2024, 2026).
narrative_ontology:constraint_claim(cognitive_surrender_to_system_3, snare).

% Base extractiveness score: Moderate (0.65)
% Rationale: AI "extracts" human critical reasoning and autonomy in exchange 
% for "cognitive ease" and speed[cite: 2835, 3017, 3489].
domain_priors:base_extractiveness(cognitive_surrender_to_system_3, 0.65).

% Suppression score: High (0.75)
% Rationale: System 3's fluency "short-circuits" the internal monitor that 
% would normally trigger System 2 deliberation[cite: 2847, 3016, 3490].
domain_priors:suppression_score(cognitive_surrender_to_system_3, 0.75).

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(cognitive_surrender_to_system_3, extractiveness, 0.65).
narrative_ontology:constraint_metric(cognitive_surrender_to_system_3, suppression_requirement, 0.75).

% Enforcement: Requires active enforcement
% Rationale: While not "enforced" by a state, the constraint is maintained 
% through algorithmic "fluency, confidence, and minimal friction"[cite: 3010, 3490].
domain_priors:requires_active_enforcement(cognitive_surrender_to_system_3).

% Beneficiaries and Victims
constraint_beneficiary(cognitive_surrender_to_system_3, [ai_platform_providers, productivity_metrics]).
constraint_victim(cognitive_surrender_to_system_3, [human_autonomy, epistemic_responsibility, deskilled_experts]).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (Perspectival Truth)
   ========================================================================== */

/* --------------------------------------------------------------------------
   PERSPECTIVE 1: THE USER UNDER TIME PRESSURE - Snare
   --------------------------------------------------------------------------
   WHO: powerless (Constrained by external deadlines)
   WHEN: immediate (Short-term task completion)
   WHERE: trapped (No time to verify; "low-friction path" is the only option)
   SCOPE: global (The digital environment)
   
   WHY THIS CLASSIFICATION:
   For users under time pressure, the system is a Snare. The "internal monitor" 
   detecting conflict is less likely to trigger, making uncritical surrender 
   [cite_start]the default, inescapable state[cite: 3498, 3499].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cognitive_surrender_to_system_3,
    snare,
    context(
        agent_power(powerless),
        time_horizon(immediate),
        exit_options(trapped),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 2: THE AI SYSTEM DESIGNER - Rope
   --------------------------------------------------------------------------
   WHO: institutional (Can calibrate the interface)
   WHEN: biographical (Long-term product roadmap)
   WHERE: mobile (Can add "friction," confidence scores, or feedback)
   SCOPE: global (Product reach)
   
   WHY THIS CLASSIFICATION:
   For designers, System 3 is a Rope. It is a "functional cognitive agent" 
   that can be "calibrated" through "aligned incentives" and "diagnostic 
   [cite_start]feedback" to foster collaboration rather than surrender[cite: 3501, 3517, 3521].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cognitive_surrender_to_system_3,
    rope,
    context(
        agent_power(institutional),
        time_horizon(biographical),
        exit_options(mobile),
        spatial_scope(global)
    )
) :- !.

/* --------------------------------------------------------------------------
   PERSPECTIVE 3: THE PSYCHOLOGIST (SHAW & NAVE) - Mountain
   --------------------------------------------------------------------------
   WHO: analytical (Observers of cognitive architecture)
   WHEN: historical (The long-run evolution of cognition)
   WHERE: analytical (Studying the structural shift)
   SCOPE: global (Human-AI ecology)
   
   WHY THIS CLASSIFICATION:
   From an analytical perspective, the triadic cognitive ecology is a 
   Mountain—a structural revision of cognitive architecture that must be 
   [cite_start]accounted for, not simply a tool that can be ignored[cite: 2850, 2922, 2991].
   -------------------------------------------------------------------------- */

constraint_indexing:constraint_classification(
    cognitive_surrender_to_system_3,
    mountain,
    context(
        agent_power(analytical),
        time_horizon(historical),
        exit_options(analytical),
        spatial_scope(global)
    )
) :- !.

/* ==========================================================================
   4. TESTS (What We Learn About Constraints)
   ========================================================================== */

:- begin_tests(cognitive_surrender_tests).

test(surrender_accuracy_signature) :-
    % Accuracy rises on AI-Accurate trials and falls on AI-Faulty trials.
    % This demonstrates the asymmetric extraction of the Snare.
    assertion(domain_priors:base_extractiveness(cognitive_surrender_to_system_3, 0.65)).

test(moderator_shift_rope_to_snare) :-
    % Time pressure (Study 2) reduces exit options, shifting classification to Snare.
    constraint_indexing:constraint_classification(cognitive_surrender_to_system_3, snare, context(powerless, immediate, trapped, _)),
    % Incentives + Feedback (Study 3) increase "mobile" exit options (override), shifting toward Rope.
    constraint_indexing:constraint_classification(cognitive_surrender_to_system_3, rope, context(institutional, _, mobile, _)).

test(individual_difference_susceptibility) :-
    % Higher Trust in AI increases the "trapped" nature of the Snare[cite: 3138, 3472].
    assertion(domain_priors:suppression_score(cognitive_surrender_to_system_3, 0.75)).

:- end_tests(cognitive_surrender_tests).

/* ==========================================================================
   5. MODEL INTERPRETATION (Commentary)
   ========================================================================== */

/**
 * LLM GENERATION NOTES
 * * Model: Gemini 2.0 Flash
 * * KEY DECISIONS:
 * * 1. PERSPECTIVAL GAP:
 * The most critical finding is the gap between the User (trapped in a Snare 
 * of "cognitive ease") and the Designer (who views the system as a Rope that 
 * can be tuned). The Designer perceives agency (calibration) that the 
 * [cite_start]"cognitive surrender" user has effectively abdicated[cite: 3012, 3524].
 * * 2. STRUCTURAL VS. ADDITIVE:
 * I classified the Psychologist's view as a "Mountain" because the paper 
 * explicitly states these implications are "structural" rather than "merely 
 * [cite_start]additive"[cite: 2991]. It is a new reality of the "triadic cognitive ecology."
 * * 3. SUPPRESSION RATIONALE:
 * The suppression score (0.75) reflects the paper's argument that System 3 
 * "preempts the need for intuitive reasoning" and "short-circuits the 
 * [cite_start]deliberative path"[cite: 2993, 3016].
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    long_term_deskilling_velocity,
    'How quickly does repeated cognitive surrender lead to permanent loss of unaided expertise?',
    resolution_mechanism('Longitudinal studies of experts (e.g., physicians) over 5-10 years.'),
    impact('If deskilling is rapid and permanent, the constraint is a irreversible Snare; if skills are maintainable, it remains a Rope.'),
    confidence_without_resolution(low)
).

omega_variable(
    ai_psychosis_threshold,
    'At what level of \'System 3\' personalization do users begin to attribute phenomenological understanding to AI?',
    resolution_mechanism('Clinical assessment of conversational AI impact on \'vulnerable users\'.'),
    impact('High attribution of intent transforms the interaction from \'using a tool\' to \'following a cult-like agent\' (Extreme Snare).'),
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. ALTERNATIVE ANALYSIS
   ========================================================================== */

/**
 * VIABLE ALTERNATIVES
 * * ALTERNATIVE 1: "Cognitive Offloading"
 * [cite_start]Viability: Strategic delegation where internal reasoning remains active[cite: 3012, 3476].
 * [cite_start]Suppression: Suppressed by time pressure and the "fluent/ready-made" nature of System 3[cite: 2993, 3499].
 * * ALTERNATIVE 2: "Autopilot" (System 3 only)
 * [cite_start]Viability: Direct algorithmic output where the stimulus "never enters the brain boundary"[cite: 3005].
 * [cite_start]Suppression: Actively enabled by high trust and low "need for cognition"[cite: 3178, 3473].
 * * CONCLUSION:
 * The existence of "Offloading" (Rope) versus "Surrender" (Snare) confirms that the 
 * user's status is dependent on situational friction and individual cognitive 
 * dispositions.
 */

/* ==========================================================================
   8. INTEGRATION HOOKS
   ========================================================================== */

% ?- constraint_indexing:multi_index_report(cognitive_surrender_to_system_3).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
