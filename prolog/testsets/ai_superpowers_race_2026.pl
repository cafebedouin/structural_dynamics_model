% ============================================================================
% CONSTRAINT STORY: ai_superpowers_race_2026
% ============================================================================
% Version: 3.4 (Deferential Realism Core)
% Logic: 3.3 (Indexed Tuple P,T,E,S)
% Generated: 2026-02-05
% ============================================================================

:- module(constraint_ai_superpowers_2026, []).

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
 * * constraint_id: ai_superpowers_2026
 * human_readable: The Sino-American AI Implementation Gap
 * domain: technological/geopolitical
 * * SUMMARY:
 * Kai-Fu Lee (01.ai) posits that while the US leads in "AGI Moonshots," 
 * China will dominate "Consumer AI" via tenacity, engineering focus, and 
 * rapid iteration. The 2026 release of "AI-First Devices"—ambient, 
 * speech-driven, and invisible—marks a transition from a "Mountain" of 
 * hardware limits to a "Rope" of agentic coordination.
 * * KEY AGENTS:
 * - Traditional Industry CIOs: Subject (Powerless - Outpaced by CAIOs)
 * - Chinese Tech Giants (ByteDance/Alibaba): Beneficiary (Institutional)
 * - Frontier Labs (OpenAI/Anthropic): Auditor (Analytical)
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Extraction is high (0.64) as companies must pivot to CAIO-led structures 
% or risk obsolescence. The "White-Glove" service extracts high fees for strategy.
domain_priors:base_extractiveness(ai_superpowers_2026, 0.64). 

% Suppression is moderate-high (0.70) as proprietary AGI models (US) 
% and tenacity-driven iteration (China) suppress slower competitors.
domain_priors:suppression_score(ai_superpowers_2026, 0.70).   

% Theater ratio is moderate (0.45). The "AGI Winner-Take-All" narrative 
% is a theatrical high-stakes story used to raise $500B in capital.
domain_priors:theater_ratio(ai_superpowers_2026, 0.45).       

% Constraint metric facts (bridge for classification engine)
narrative_ontology:constraint_metric(ai_superpowers_2026, extractiveness, 0.64).
narrative_ontology:constraint_metric(ai_superpowers_2026, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(ai_superpowers_2026, theater_ratio, 0.45).

% Constraint classification claim
narrative_ontology:constraint_claim(ai_superpowers_2026, scaffold).

% Primary keys for the classification engine
% High-extraction stakeholders
narrative_ontology:constraint_beneficiary(ai_superpowers_2026, chief_ai_officers).
narrative_ontology:constraint_victim(ai_superpowers_2026, conservative_cios).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   ========================================================================== */

% PERSPECTIVE 1: THE TRADITIONAL CEO (SNARE)
% For companies in banking or mining, AI is a Snare: an inescapable 
% requirement for transformation that they are only 1% prepared to execute.
constraint_indexing:constraint_classification(ai_superpowers_2026, snare, 
    context(agent_power(powerless), 
            time_horizon(biographical), 
            exit_options(trapped), 
            spatial_scope(global))).

% PERSPECTIVE 2: THE CHINESE TECH GIANT (ROPE)
% Giants like Tencent and Alibaba view AI as a Rope: essential coordination 
% to extend existing apps (WeChat, Douyin) and win the consumer market.
constraint_indexing:constraint_classification(ai_superpowers_2026, rope, 
    context(agent_power(institutional), 
            time_horizon(generational), 
            exit_options(mobile), 
            spatial_scope(national))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (SCAFFOLD)
% Analysts view "AI-First Devices" as a Scaffold: a transitional 
% hardware moment (Nokia vs. iPhone) that will eventually sunset.
constraint_indexing:constraint_classification(ai_superpowers_2026, scaffold,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))) :-
    narrative_ontology:has_sunset_clause(ai_superpowers_2026).

% Mandatory sunset clause for the transitional device phase
narrative_ontology:has_sunset_clause(ai_superpowers_2026).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_superpowers_2026_tests).

test(perspectival_gap) :-
    constraint_indexing:constraint_classification(ai_superpowers_2026, snare, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_superpowers_2026, rope, context(agent_power(institutional), _, _, _)).

test(theater_check) :-
    domain_priors:theater_ratio(ai_superpowers_2026, TR),
    TR > 0.40.

:- end_tests(ai_superpowers_2026_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * The extraction score (0.64) is driven by the need for "White-Glove" AI 
 * consulting and the replacement of CIOs with CAIOs. The Theater Ratio 
 * (0.45) reflects the "Step Function" AGI narrative used to justify massive 
 * $50bn+ investments in the US, contrasted by the "Study Group" approach 
 * in China.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    omega_agi_step_function,
    'Will AGI arrive as a winner-take-all step function (US) or a linear trajectory (China)?',
    'Comparison of 2026-2027 enterprise vs. consumer productivity outcomes.',
    'Step function implies a permanent AGI Mountain; Linear implies a competitive Rope.',
    confidence_without_resolution(low)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(ai_superpowers_2026, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio rises as the AGI-or-Bust narrative intensifies in US funding.
narrative_ontology:measurement(ai_tr_t0, ai_superpowers_2026, theater_ratio, 0, 0.25).
narrative_ontology:measurement(ai_tr_t5, ai_superpowers_2026, theater_ratio, 5, 0.35).
narrative_ontology:measurement(ai_tr_t10, ai_superpowers_2026, theater_ratio, 10, 0.45).

% Extraction rises as companies are forced to hire CAIOs and pay performance fees.
narrative_ontology:measurement(ai_ex_t0, ai_superpowers_2026, base_extractiveness, 0, 0.30).
narrative_ontology:measurement(ai_ex_t5, ai_superpowers_2026, base_extractiveness, 5, 0.50).
narrative_ontology:measurement(ai_ex_t10, ai_superpowers_2026, base_extractiveness, 10, 0.64).

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
