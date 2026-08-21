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
 *   This constraint describes ritual as a mechanism for encoding and
 *   transmitting operational survival competence, focusing on its functional
 *   yield in pattern recognition, resource coordination, and threat
 *   assessment rehearsal. Examples include Passover's rapid-departure
 *   readiness drills or Tisha B'Av's resource-scarcity training. This is one
 *   reading of the 'catastrophe_memory_transmission' kernel, emphasizing the
 *   practical, adaptive function of ritual over its purely symbolic or
 *   identity-preserving aspects. The constraint is claimed as a Rope,
 *   reflecting its genuine coordination function in collective survival.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_transmission__operational_competence_reading, 0.15).
domain_priors:suppression_score(catastrophe_memory_transmission__operational_competence_reading, 0.2).
domain_priors:theater_ratio(catastrophe_memory_transmission__operational_competence_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__operational_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__operational_competence_reading, "Catastrophe Memory Transmission (Operational Competence Reading)").
narrative_ontology:topic_domain(catastrophe_memory_transmission__operational_competence_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__operational_competence_reading, '907e41af-c690-4350-9d81-a806e4798270').
narrative_ontology:cs_kernel_codification('907e41af-c690-4350-9d81-a806e4798270', implicit).
narrative_ontology:cs_authority_grounding('907e41af-c690-4350-9d81-a806e4798270', practice).
narrative_ontology:cs_interpretation_layer_present('907e41af-c690-4350-9d81-a806e4798270').
narrative_ontology:cs_reading_relation('907e41af-c690-4350-9d81-a806e4798270', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('907e41af-c690-4350-9d81-a806e4798270', catastrophe_memory_transmission__hybrid_embedded_reading, coexists_with).
narrative_ontology:cs_axiom('907e41af-c690-4350-9d81-a806e4798270', foundational, ritual_as_operational_rehearsal).
narrative_ontology:cs_axiom_status(ritual_as_operational_rehearsal, holdable).
narrative_ontology:cs_axiom_grounding('907e41af-c690-4350-9d81-a806e4798270', ritual_as_operational_rehearsal, empirically_contingent).
narrative_ontology:cs_axiom('907e41af-c690-4350-9d81-a806e4798270', foundational, survival_competence_as_primary_yield).
narrative_ontology:cs_axiom_status(survival_competence_as_primary_yield, holdable).
narrative_ontology:cs_axiom_grounding('907e41af-c690-4350-9d81-a806e4798270', survival_competence_as_primary_yield, instrumental).
narrative_ontology:cs_reference_frame('907e41af-c690-4350-9d81-a806e4798270', functional_adaptive_practice).
narrative_ontology:cs_drift_state('907e41af-c690-4350-9d81-a806e4798270', contemporary_secular_context, gap(practice_drift, minor, true)).
narrative_ontology:cs_created_at('907e41af-c690-4350-9d81-a806e4798270', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, future_generations).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, community_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in rituals that rehearse practical responses to historical catastrophes, thereby acquiring and transmitting survival skills and knowledge. They benefit from enhanced collective resilience but are constrained by the ritual's prescribed forms.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, community_members, beneficiary,
    organized, generational, constrained, local).

% Are the ultimate beneficiaries of the operational competence transmitted through ritual, inheriting a collective memory of survival strategies. They have no agency in shaping the ritual but depend on its effective transmission.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, future_generations, beneficiary,
    powerless, generational, trapped, local).

% Design, interpret, and lead rituals, ensuring their fidelity to the operational lessons of past catastrophes. They maintain the constraint by guiding the community through the prescribed actions and narratives.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_leaders, agenda_setter,
    institutional, biographical, constrained, local).

% Study the functional aspects of ritual in transmitting survival competence, evaluating its efficacy in preparing communities for future challenges. They are external to the ritual's practice but analyze its structural properties.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective action and knowledge transfer across generations to enhance survival capacity in the face of recurring threats, by encoding practical lessons into repeatable ritual forms.
% TRANSFER_FUNCTION: Transfers operational knowledge, pattern recognition skills, and resource coordination strategies from past generations to present and future community members, ensuring collective preparedness.
% ABSENT_VOICES: Those who prioritize purely symbolic or aesthetic aspects of ritual might object that the operational focus diminishes the spiritual or cultural value. They are often present but their interpretations are subordinated to the functional imperative.
% DISAPPEARANCE_RATIONALE: If the ritual's operational competence transmission vanished, communities would lose a vital mechanism for collective learning and preparedness, potentially leading to repeated catastrophic failures and a significant reduction in long-term survival capacity. The social fabric would lose a key adaptive function.
% FOUNDING_PROBLEM: Communities faced recurring existential threats (e.g., famine, invasion, natural disaster) and needed a reliable, intergenerational mechanism to transmit practical survival knowledge and coordinate responses.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies and historical records from outside the immediate community corroborate the efficacy of such rituals in fostering resilience and transmitting practical skills, demonstrating their continued relevance in preparing for ongoing or analogous threats.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__operational_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__operational_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__operational_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_transmission__operational_competence_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_transmission__operational_competence_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.15) because the primary 'cost' is participation and adherence to ritual forms, which is directly offset by the benefit of enhanced survival competence. Suppression is also low (0.2) as adherence is largely voluntary, driven by perceived utility and communal benefit rather than coercion. Theater ratio is low (0.1) because the ritual's elements are primarily functional, even if symbolically rich. Accessibility collapse is moderate (0.7) as alternative methods for transmitting such complex, embodied knowledge are limited, making ritual a highly effective, if not unique, solution. Resistance is low (0.05) because the operational benefits are widely recognized and valued by the community.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of community members, the ritual is a vital tool for survival, a clear Rope. From an analytical observer's perspective, the constraint's low extractiveness and high coordination function also align with a Rope, or potentially a Mountain if the transmission of such competence is seen as an irreducible requirement for collective survival.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members and future generations are direct beneficiaries, gaining survival competence. Ritual leaders act as agenda-setters, guiding the transmission process. There are no identifiable 'victims' in this reading, as any 'cost' (e.g., time, adherence) is directly tied to the operational benefit. The constraint subsidizes collective resilience.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    operational_vs_symbolic_efficacy,
    'To what extent is the observed operational competence a direct result of the ritual''s practical elements, versus an indirect effect of symbolic cohesion and identity reinforcement?',
    'Comparative studies of communities with similar rituals but varying degrees of explicit operational focus, or experimental interventions altering ritual elements to isolate functional components.',
    'If operational competence is largely an indirect effect of symbolic cohesion, this reading''s extractiveness might be underestimated, as the ''cost'' of symbolic adherence (which is higher) would be a more significant factor. This would push the classification towards a Tangled Rope or Snare if the symbolic elements are used to extract from participants without direct operational yield.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_vs_symbolic_efficacy, empirical, 'Distinguishing direct operational yield from indirect symbolic effects in ritual efficacy.').

omega_variable(
    natural_law_vs_social_construct,
    'Is the transmission of catastrophe memory through ritual a universal, near-natural law of collective survival, or a socially constructed coordination mechanism?',
    'Cross-cultural and historical analysis of diverse societies'' responses to catastrophe: if similar functional patterns emerge independently, it supports a natural law interpretation. If patterns are highly contingent on cultural context, it supports a social construct.',
    'If a natural law, the constraint would be reclassified as a Mountain, with negligible extractiveness and suppression. If a social construct, its Rope classification would be reinforced, emphasizing its constructed nature as a coordination solution.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_social_construct, conceptual, 'Assessing the naturalness of ritual as a catastrophe memory transmission mechanism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__operational_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0, 0.08).
narrative_ontology:measurement(cata_tr_t25, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 25, 0.09).
narrative_ontology:measurement(cata_tr_t50, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 50, 0.1).
narrative_ontology:measurement(cata_tr_t75, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 75, 0.09).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(cata_be_t25, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 25, 0.12).
narrative_ontology:measurement(cata_be_t50, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 50, 0.15).
narrative_ontology:measurement(cata_be_t75, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 75, 0.14).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0, 0.15).
narrative_ontology:measurement(cata_su_t25, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 25, 0.17).
narrative_ontology:measurement(cata_su_t50, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 50, 0.2).
narrative_ontology:measurement(cata_su_t75, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 75, 0.19).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 100, 0.2).


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
