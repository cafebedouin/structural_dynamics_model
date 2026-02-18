% ============================================================================
% CONSTRAINT STORY: ai_banal_capture
% ============================================================================
% Version: 5.2 (Deferential Realism Core + Boltzmann + Purity + Network)
% Logic: 5.2 (Indexed Tuple P,T,E,S + Coupling + Purity + Network Drift)
% Generated: 2024-07-20
% ============================================================================

:- module(constraint_ai_banal_capture, []).

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
    narrative_ontology:interval/3,
    narrative_ontology:measurement/5,
    narrative_ontology:constraint_metric/3,
    narrative_ontology:constraint_beneficiary/2,
    narrative_ontology:constraint_victim/2,
    narrative_ontology:constraint_claim/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    narrative_ontology:boltzmann_floor_override/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 * * constraint_id: ai_banal_capture
 * human_readable: The Banal Cognitive Engine
 * domain: technological/social
 * * SUMMARY:
 * A transition where A.I. moves from a "magic" breakthrough to a "banal"
 * background utility like GPS or spreadsheets. The constraint
 * functions by automating cognitive operations and creating a dependency,
 * extracting value from the cognitive labor it displaces or augments.
 * * KEY AGENTS:
 * - Knowledge Workers & Students: Subject (Powerless) - At risk of relying on "mimicry
 * machines" rather than developing critical thinking, leading to skill atrophy.
 * - AI Firms & Platform Owners: Beneficiary (Institutional) - Automating their own
 * operations and capturing value from the widespread adoption of their tools.
 * - The Risk Assessor: Auditor (Analytical) - Monitoring catastrophic
 * risks, emergent abilities, and societal-scale dependencies.
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% Numerical anchors for v3.4 thresholds
domain_priors:base_extractiveness(ai_banal_capture, 0.68). % High, as AI captures value from creative and cognitive labor.
domain_priors:suppression_score(ai_banal_capture, 0.75).   % High due to network effects and ubiquity; opting out becomes costly.
domain_priors:theater_ratio(ai_banal_capture, 0.58).       % Moderate, reflecting the persistent narrative of "magic" vs. the reality of pattern-matching.

% Constraint metric facts — primary keys used by the classification engine.
narrative_ontology:constraint_metric(ai_banal_capture, extractiveness, 0.68).
narrative_ontology:constraint_metric(ai_banal_capture, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(ai_banal_capture, theater_ratio, 0.58).

% Constraint self-claim (what does the constraint claim to be?)
% AI is marketed as a coordination/productivity tool.
narrative_ontology:constraint_claim(ai_banal_capture, tangled_rope).
narrative_ontology:human_readable(ai_banal_capture, "The Banal Cognitive Engine").
narrative_ontology:topic_domain(ai_banal_capture, "technological/social").

% Binary flags
domain_priors:requires_active_enforcement(ai_banal_capture). % Enforcement via platform terms, API access rules, and market dominance.

% Structural property derivation hooks:
% These are required for Tangled Rope classification.
narrative_ontology:constraint_beneficiary(ai_banal_capture, ai_firms_and_platform_owners).
narrative_ontology:constraint_victim(ai_banal_capture, knowledge_workers_and_students).

/* ==========================================================================
   3. INDEXED CLASSIFICATIONS (P, T, E, S)
   χ = ε × π(P) × σ(S)
   Power (P) and Scope (S) both affect effective extraction.
   Scope modifiers: local=0.8, regional=0.9, national=1.0,
                    continental=1.1, global=1.2, universal=1.0.
   ========================================================================== */

% PERSPECTIVE 1: THE SUBJECT (SNARE)
% The student or worker perceives a snare: shortcuts are tempting, leading to
% dependency and skill erosion. The high extraction is felt directly.
constraint_indexing:constraint_classification(ai_banal_capture, snare,
    context(agent_power(powerless),
            time_horizon(biographical),
            exit_options(trapped),
            spatial_scope(global))).

% PERSPECTIVE 2: THE BENEFICIARY (ROPE)
% Firms view their AI platforms as a rope, a pure coordination and productivity
% tool that powers innovation and efficiency. Extraction is invisible or justified.
constraint_indexing:constraint_classification(ai_banal_capture, rope,
    context(agent_power(institutional),
            time_horizon(generational),
            exit_options(mobile),
            spatial_scope(global))).

% PERSPECTIVE 3: THE ANALYTICAL OBSERVER (TANGLED ROPE)
% The analyst sees both the coordination function (a genuine utility) and the
% asymmetric extraction of value and cognitive autonomy.
constraint_indexing:constraint_classification(ai_banal_capture, tangled_rope,
    context(agent_power(analytical),
            time_horizon(historical),
            exit_options(analytical),
            spatial_scope(global))).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(ai_banal_capture_tests).

test(perspectival_gap) :-
    % Verify there is a perspectival gap between powerless and institutional.
    constraint_indexing:constraint_classification(ai_banal_capture, TypePowerless, context(agent_power(powerless), _, _, _)),
    constraint_indexing:constraint_classification(ai_banal_capture, TypeInstitutional, context(agent_power(institutional), _, _, _)),
    TypePowerless == snare,
    TypeInstitutional == rope,
    TypePowerless \= TypeInstitutional.

test(analytical_classification_is_tangled_rope) :-
    % Verify the analytical observer correctly identifies the hybrid nature.
    constraint_indexing:constraint_classification(ai_banal_capture, tangled_rope, context(agent_power(analytical), _, _, _)).

test(tangled_rope_structural_requirements_met) :-
    % Verify all three structural properties for Tangled Rope are present.
    narrative_ontology:constraint_beneficiary(ai_banal_capture, _), % -> has_coordination_function
    narrative_ontology:constraint_victim(ai_banal_capture, _),    % -> has_asymmetric_extraction
    domain_priors:requires_active_enforcement(ai_banal_capture).

:- end_tests(ai_banal_capture_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 * This constraint models the societal integration of powerful AI. The high
 * base extractiveness (0.68) reflects the value captured from automating
 * cognitive tasks previously done by humans. The high suppression (0.75)
 * represents the powerful network effects and switching costs that make
 * "opting out" of the ecosystem increasingly difficult.
 *
 * The Perspectival Gap is stark: for the institutional beneficiary (AI firm),
 * the system is a Rope, a tool for coordination and productivity. For the
 * powerless subject (student, knowledge worker), it is a Snare, creating
 * dependency and eroding skills.
 *
 * * MANDATROPHY ANALYSIS:
 * The Tangled Rope classification is critical here. It prevents the system from
 * making a binary choice between "useful tool" (Rope) and "exploitative trap"
 * (Snare). It correctly identifies that the constraint is simultaneously both:
 * it provides a genuine coordination function (making information accessible,
 * automating tasks) while also imposing a highly asymmetric extraction of
 * value and autonomy. This resolves the Mandatrophy by acknowledging the
 * dual nature of the system.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

% omega_variable(ID, Question, Resolution_Mechanism, Impact, Confidence).
omega_variable(
    omega_ai_banal_capture,
    'Will general AI capabilities plateau at "mimicry machine" levels or achieve flexible, novel reasoning?',
    'Empirical testing of flexible reasoning on novel, out-of-distribution problems over the next decade.',
    'If it plateaus: a persistent, high-extraction Tangled Rope. If it achieves general reasoning: could become a permanent Mountain of cognitive hierarchy.',
    confidence_without_resolution(medium)
).

/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

% Required for external script parsing
narrative_ontology:interval(ai_banal_capture, 0, 10).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Temporal data shows the transition from a novel technology with high
% "magic" narratives to a banal utility where extraction becomes normalized.
%
% Theater ratio over time (from "Magic" to "Banal Utility"):
narrative_ontology:measurement(ai_banal_capture_tr_t0, ai_banal_capture, theater_ratio, 0, 0.30).
narrative_ontology:measurement(ai_banal_capture_tr_t5, ai_banal_capture, theater_ratio, 5, 0.45).
narrative_ontology:measurement(ai_banal_capture_tr_t10, ai_banal_capture, theater_ratio, 10, 0.58).

% Extraction over time (from niche tool to substantial automation):
narrative_ontology:measurement(ai_banal_capture_ex_t0, ai_banal_capture, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(ai_banal_capture_ex_t5, ai_banal_capture, base_extractiveness, 5, 0.55).
narrative_ontology:measurement(ai_banal_capture_ex_t10, ai_banal_capture, base_extractiveness, 10, 0.68).

/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA (v5.0-5.2)
   ========================================================================== */

% Coordination type: AI platforms function as a new layer of global infrastructure.
narrative_ontology:coordination_type(ai_banal_capture, global_infrastructure).

% Network relationships: The rise of banal AI directly impacts the structure
% of the labor market and credentialing systems.
narrative_ontology:affects_constraint(ai_banal_capture, labor_market_precarity).
narrative_ontology:affects_constraint(ai_banal_capture, education_credentialism).


/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */