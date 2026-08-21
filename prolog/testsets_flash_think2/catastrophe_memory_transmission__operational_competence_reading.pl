% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_transmission__operational_competence_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_transmission__operational_competence_reading, []).

:- use_module(constraint_indexing).
:- use_module(domain_priors).
:- use_module(narrative_ontology).

% --- Constraint Identity Rule (DP-001: ε-Invariance) ---
% Each constraint story must have a single, stable base extractiveness (ε).
% If changing the observable used to evaluate this constraint would change ε,
% you are looking at two distinct constraints. Write separate .pl files for
% each, link them with affects_constraint/2, and document the relationship
% in both files' narrative context sections.
%
% The context tuple is CLOSED at arity 4: (P, T, E, S).
% Do not add measurement_basis, beneficiary/victim, or any other arguments.
% Linter Rule 23 enforces context/4.
%
% See: epsilon_invariance_principle.md

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    narrative_ontology:stakeholder_gain_flow/2,
    narrative_ontology:fixing_cost_class/2,
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
    narrative_ontology:cs_interpretation_layer_present/1,
    narrative_ontology:cs_kernel_id/2,
    narrative_ontology:cs_reading_relation/3,
    narrative_ontology:cs_axiom/3,
    narrative_ontology:cs_axiom_status/2,
    narrative_ontology:cs_axiom_grounding/3,
    narrative_ontology:cs_reference_frame/2,
    narrative_ontology:cs_drift_state/3,
    narrative_ontology:cs_created_at/2,
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: catastrophe_memory_transmission__operational_competence_reading
 *   human_readable: Catastrophe Memory Transmission (Operational Competence Reading)
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint story instantiates the 'operational competence' reading
 *   of catastrophe memory transmission via ritual. It focuses on how ritual
 *   encodes and transmits practical survival skills, pattern recognition for
 *   threats, and resource coordination strategies across generations. Ritual
 *   elements are evaluated by their operational yield, such as Passover's
 *   rapid-departure readiness or Tisha B'Av's resource-scarcity training. The
 *   constraint functions as a coordination mechanism for future survival
 *   capacity.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__operational_competence_reading, 0.22).
domain_priors:suppression_score(catastrophe_memory_transmission__operational_competence_reading, 0.28).
domain_priors:theater_ratio(catastrophe_memory_transmission__operational_competence_reading, 0.18).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, extractiveness, 0.22).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0.28).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0.18).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, accessibility_collapse, 0.45).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__operational_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__operational_competence_reading, "Catastrophe Memory Transmission (Operational Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_transmission__operational_competence_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__operational_competence_reading, '2caf54c9-400c-44b4-84c0-97ad2772548e').
narrative_ontology:cs_kernel_codification('2caf54c9-400c-44b4-84c0-97ad2772548e', implicit).
narrative_ontology:cs_authority_grounding('2caf54c9-400c-44b4-84c0-97ad2772548e', practice).
narrative_ontology:cs_interpretation_layer_present('2caf54c9-400c-44b4-84c0-97ad2772548e').
narrative_ontology:cs_reading_relation('2caf54c9-400c-44b4-84c0-97ad2772548e', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('2caf54c9-400c-44b4-84c0-97ad2772548e', catastrophe_memory_transmission__hybrid_embedded_reading, coexists_with).
narrative_ontology:cs_axiom('2caf54c9-400c-44b4-84c0-97ad2772548e', foundational, ritual_as_survival_technology).
narrative_ontology:cs_axiom_status(ritual_as_survival_technology, holdable).
narrative_ontology:cs_axiom_grounding('2caf54c9-400c-44b4-84c0-97ad2772548e', ritual_as_survival_technology, empirically_contingent).
narrative_ontology:cs_axiom('2caf54c9-400c-44b4-84c0-97ad2772548e', secondary, competence_is_measurable).
narrative_ontology:cs_axiom_status(competence_is_measurable, holdable).
narrative_ontology:cs_axiom_grounding('2caf54c9-400c-44b4-84c0-97ad2772548e', competence_is_measurable, empirically_contingent).
narrative_ontology:cs_reference_frame('2caf54c9-400c-44b4-84c0-97ad2772548e', functional_transmission_paradigm).
narrative_ontology:cs_drift_state('2caf54c9-400c-44b4-84c0-97ad2772548e', contemporary_secular_era, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('2caf54c9-400c-44b4-84c0-97ad2772548e', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, community_members).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, future_generations).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__operational_competence_reading, those_who_mistake_symbol_for_substance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in rituals that encode survival competence, benefiting from enhanced collective preparedness and resilience. They invest time and effort in learning and performing the rituals.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, community_members, beneficiary,
    moderate, biographical, constrained, local).

% Inherit the accumulated survival competence and preparedness, or the lack thereof, from past generations. They are direct beneficiaries of effective transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, future_generations, beneficiary,
    powerless, civilizational, trapped, universal).

% Administer, teach, and interpret the rituals, ensuring their accurate transmission and adaptation. They are responsible for maintaining the operational fidelity of the practices.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_leaders, agenda_setter,
    institutional, generational, constrained, local).

% Engage with the ritual but misinterpret its core function, focusing solely on symbolic or aesthetic aspects rather than the underlying operational competence. They pay an opportunity cost by failing to extract the full survival utility.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, those_who_mistake_symbol_for_substance, payer,
    powerless, immediate, identity_locked, local).

% Study the phenomenon of ritual and its role in collective memory and survival, seeking to understand its mechanisms and efficacy from an external, academic perspective.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_transmission__operational_competence_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_transmission__operational_competence_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Transmits critical survival knowledge, behavioral patterns, and collective memory of past threats across generations, enabling collective response to future catastrophes.
% TRANSFER_FUNCTION: Transfers practical knowledge, behavioral scripts, and threat assessment capabilities from experienced generations to new ones, enhancing collective resilience.
% ABSENT_VOICES: Those who dismiss ritual as mere superstition or purely symbolic, thereby missing its operational utility, are epistemically excluded from recognizing the constraint's function.
% DISAPPEARANCE_RATIONALE: If this mechanism for transmitting survival competence vanished, communities would lose a vital, integrated system for preparing for and responding to existential threats, leading to increased vulnerability and potential collapse in the face of future catastrophes.
% FOUNDING_PROBLEM: The recurring threat of catastrophe (e.g., famine, plague, invasion, environmental disaster) and the challenge of transmitting hard-won survival lessons across generations without direct experience.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of disaster-prone communities, historical accounts of ritual's role in resilience, and cognitive science research on embodied learning and memory corroborate the ongoing relevance of this problem and ritual's role in addressing it.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__operational_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__operational_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__operational_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_transmission__operational_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__operational_competence_reading, 0.22, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_transmission__operational_competence_reading_tests).
:- end_tests(catastrophe_memory_transmission__operational_competence_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Rope due to its primary function as an effective coordination mechanism for intergenerational survival competence. Extractiveness is low (0.22) because the 'cost' of ritual participation is largely offset by the collective benefit of enhanced preparedness. Suppression is low (0.28) as participation is generally voluntary, driven by perceived utility and social cohesion rather than coercion. Theater ratio is low (0.18) because the focus is on the functional, operational aspects of the ritual, with performance serving to reinforce practical lessons rather than obscure a lack of function. Accessibility collapse is moderate (0.45) as other forms of knowledge transmission exist, but ritual offers a uniquely integrated and resilient method.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of this reading, the ritual's value lies in its demonstrable operational yield for survival. Other readings might emphasize symbolic continuity or the inseparability of symbol and competence, leading to different evaluations of its function and beneficiaries. The engine's classification will highlight how this operational focus shapes the constraint's structural properties.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members and future generations are direct beneficiaries, receiving the survival competence. Ritual leaders act as agenda-setters, guiding the transmission. Those who mistake symbol for substance are payers of an opportunity cost, failing to fully utilize the ritual's operational benefits due to misinterpretation.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate of transmitting survival competence remains critically live, as communities continue to face existential threats. The constraint's low extractiveness and high coordination function prevent it from being mislabeled as a Snare or Piton, as its utility is actively realized and its founding problem persists.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_context,
    'This constraint is one reading of the ''catastrophe_memory_transmission'' kernel. How would its classification change if a sibling reading were adopted?',
    'Analyze the structural properties (extractiveness, beneficiaries, victims) of the ''symbol_continuity_reading'' and ''hybrid_embedded_reading'' to compare their classifications.',
    'The ''symbol_continuity_reading'' might classify as a Rope (for identity coordination) or even a Piton (if symbolic forms persist without active meaning). The ''hybrid_embedded_reading'' might classify as a Tangled Rope if the inseparability claim masks extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_context, conceptual, 'Impact of alternative kernel readings on classification.').

omega_variable(
    operational_yield_measurement,
    'How can the ''operational yield'' of ritual in transmitting survival competence be empirically measured and quantified?',
    'Longitudinal ethnographic studies, comparative analysis of disaster outcomes in communities with varying ritual practices, and cognitive science experiments on embodied memory and skill transfer.',
    'Clear empirical metrics for operational yield would strengthen the ''rope'' classification by demonstrating tangible benefits, or shift it towards ''piton'' if the yield is negligible despite claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_yield_measurement, empirical, 'Empirical validation of ritual''s operational effectiveness.').

omega_variable(
    misinterpretation_as_cost,
    'Is the ''cost'' borne by those who mistake symbol for substance a form of extraction, or merely a missed opportunity for benefit?',
    'Examine whether the misinterpretation is actively fostered or exploited by the agenda-setters for their benefit, or if it arises from individual cognitive biases or cultural shifts.',
    'If actively exploited, it would increase the constraint''s effective extractiveness and push it towards a ''tangled_rope'' or ''snare'' classification. If a missed opportunity, the ''rope'' classification remains appropriate.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(misinterpretation_as_cost, conceptual, 'Nature of cost from misinterpreting ritual''s operational function.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__operational_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 20, 0.16).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 40, 0.17).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 60, 0.18).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 80, 0.19).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 100, 0.18).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 20, 0.21).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 40, 0.22).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 60, 0.22).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 80, 0.23).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 100, 0.22).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0, 0.25).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 20, 0.26).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 40, 0.27).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 60, 0.28).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 80, 0.29).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 100, 0.28).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_transmission__operational_competence_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
