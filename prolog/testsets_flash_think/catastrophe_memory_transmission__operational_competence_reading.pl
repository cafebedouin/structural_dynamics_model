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
 *   human_readable: Ritual as Catastrophe Operational Competence Transmission
 *   domain: religious_studies/collective_memory/ritual_studies
 *
 * SUMMARY:
 *   This constraint describes the function of ritual in encoding and
 *   transmitting survival competence, focusing on its operational yield. It
 *   is the 'operational_competence_reading' of the
 *   'catastrophe_memory_transmission' kernel. Sibling readings include
 *   'symbol_continuity_reading' (ritual as identity preservation) and
 *   'hybrid_embedded_reading' (competence embedded within symbolic form).
 *   This reading emphasizes the practical, adaptive function of ritual
 *   elements, such as rehearsing rapid departure or resource coordination, as
 *   seen in practices like Passover or Tisha B'Av.
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
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_transmission__operational_competence_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_transmission__operational_competence_reading, rope).
narrative_ontology:human_readable(catastrophe_memory_transmission__operational_competence_reading, "Ritual as Catastrophe Operational Competence Transmission").
narrative_ontology:topic_domain(catastrophe_memory_transmission__operational_competence_reading, "religious_studies/collective_memory/ritual_studies").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_transmission__operational_competence_reading, '800084b0-85ff-40bb-be82-5e64bd312365').
narrative_ontology:cs_kernel_codification('800084b0-85ff-40bb-be82-5e64bd312365', implicit).
narrative_ontology:cs_authority_grounding('800084b0-85ff-40bb-be82-5e64bd312365', practice).
narrative_ontology:cs_interpretation_layer_present('800084b0-85ff-40bb-be82-5e64bd312365').
narrative_ontology:cs_reading_relation('800084b0-85ff-40bb-be82-5e64bd312365', catastrophe_memory_transmission__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('800084b0-85ff-40bb-be82-5e64bd312365', catastrophe_memory_transmission__hybrid_embedded_reading, influences).
narrative_ontology:cs_axiom('800084b0-85ff-40bb-be82-5e64bd312365', foundational, operational_yield_is_primary).
narrative_ontology:cs_axiom_status(operational_yield_is_primary, holdable).
narrative_ontology:cs_axiom_grounding('800084b0-85ff-40bb-be82-5e64bd312365', operational_yield_is_primary, empirically_contingent).
narrative_ontology:cs_axiom('800084b0-85ff-40bb-be82-5e64bd312365', foundational, competence_is_transmissible_via_pattern).
narrative_ontology:cs_axiom_status(competence_is_transmissible_via_pattern, holdable).
narrative_ontology:cs_axiom_grounding('800084b0-85ff-40bb-be82-5e64bd312365', competence_is_transmissible_via_pattern, empirically_contingent).
narrative_ontology:cs_reference_frame('800084b0-85ff-40bb-be82-5e64bd312365', ancestral_competence_transmission).
narrative_ontology:cs_drift_state('800084b0-85ff-40bb-be82-5e64bd312365', contemporary_secular_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('800084b0-85ff-40bb-be82-5e64bd312365', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_transmission__operational_competence_reading, catastrophe_memory_transmission).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, future_generations).
narrative_ontology:constraint_beneficiary(catastrophe_memory_transmission__operational_competence_reading, community_members).
narrative_ontology:constraint_victim(catastrophe_memory_transmission__operational_competence_reading, those_who_mistake_symbol_for_substance).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Participate in rituals, investing time and effort, and in return gain embodied knowledge, pattern recognition skills, and rehearsed responses crucial for collective survival in the face of potential catastrophes. Their exit is constrained by social ties and the perceived value of the competence.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, community_members, beneficiary,
    moderate, biographical, constrained, local).

% Are the ultimate beneficiaries, inheriting the survival competence transmitted through ritual. They have no agency in the creation or maintenance of the ritual but depend on its successful transmission for their future resilience.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, future_generations, beneficiary,
    powerless, generational, trapped, universal).

% Are responsible for the accurate transmission, interpretation, and adaptation of rituals. They ensure the operational competence embedded within the practices remains relevant and effective, investing significant personal and social capital in this role.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, ritual_leaders, agenda_setter,
    organized, generational, constrained, local).

% Are conceptually 'victims' in that they may participate in rituals but fail to grasp the underlying operational competence, focusing instead on superficial symbolic meaning. This leads to a deficit in actual survival readiness, a cost borne by themselves and potentially the community. Their 'identity_lock' is to a particular, incomplete interpretation.
narrative_ontology:constraint_stakeholder(catastrophe_memory_transmission__operational_competence_reading, those_who_mistake_symbol_for_substance, payer,
    powerless, immediate, identity_locked, local).

% Study the mechanisms of ritual and collective memory, seeking to understand how operational competence is encoded and transmitted. They are external to the ritual's direct operation but provide critical analysis of its efficacy and structural properties.
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
% COORDINATION_FUNCTION: Transmits complex, embodied survival knowledge, pattern recognition skills, and adaptive behaviors across generations, enabling collective response to future catastrophes by coordinating shared understanding and rehearsed actions.
% TRANSFER_FUNCTION: Transfers practical knowledge, embodied skills, and collective memory of past threats and effective responses from previous generations to present and future community members through patterned action and narrative.
% ABSENT_VOICES: Individuals or groups prioritizing immediate gratification, individualistic survival, or purely propositional knowledge might object to the discipline, time commitment, and non-explicit nature of ritual transmission. They are often self-excluded or marginalized from communities that value this form of competence.
% DISAPPEARANCE_RATIONALE: If rituals encoding catastrophe memory and operational competence vanished, the specific, embodied knowledge and collective memory of past threats would degrade. Future generations would be less prepared for similar events, leading to higher casualties, slower recovery, and a loss of cultural resilience, fundamentally reorganizing the community's adaptive capacity.
% FOUNDING_PROBLEM: Communities faced recurrent existential threats (e.g., famine, invasion, natural disaster) and needed a robust, resilient, and culturally embedded mechanism to transmit hard-won survival lessons and adaptive behaviors beyond explicit instruction, ensuring long-term collective resilience.
% FOUNDING_PROBLEM_CORROBORATION: Anthropological studies of resilient communities, historical accounts of disaster response, and cognitive science research on embodied learning and collective memory corroborate the efficacy of ritual in transmitting non-propositional competence. These external analyses support the claim that the founding problem of intergenerational competence transmission remains relevant and actively addressed by such rituals.
narrative_ontology:disappearance_verdict(catastrophe_memory_transmission__operational_competence_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_transmission__operational_competence_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_transmission__operational_competence_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
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
 *   The base extractiveness is low (0.15) because the primary function is to provide a collective benefit (survival competence) rather than to extract resources. Any 'cost' is the necessary investment in learning and participation. Suppression is low (0.2) as participation is largely voluntary, driven by perceived utility and social cohesion, rather than coercion. Alternatives for such deep, embodied, intergenerational competence transmission are limited, leading to moderate accessibility collapse (0.65). The theater ratio is low (0.1) because the actions, while symbolic, are understood to have a functional, operational purpose. Resistance is low (0.1) as the ritual is generally seen as beneficial for community resilience.
 *
 * PERSPECTIVAL GAP:
 *   Ritual leaders and community members who fully grasp the operational aspect of the ritual perceive it as a vital coordination mechanism. Those who focus solely on the symbolic or aesthetic aspects might miss its deeper functional value, experiencing a 'cost' of misdirection. Analytical observers aim to bridge this gap by articulating the underlying mechanisms.
 *
 * DIRECTIONALITY LOGIC:
 *   Community members and future generations are the primary beneficiaries, gaining crucial survival competence. Ritual leaders act as agenda-setters, guiding the transmission process. Those who mistake symbol for substance are conceptual victims, bearing the cost of incomplete competence. The constraint subsidizes the community's long-term survival capacity.
 *
 * MANDATROPHY ANALYSIS:
 *   The mandate for this constraint remains live ('founding_problem_status': 'live') because the underlying problem of transmitting survival competence across generations in the face of recurrent threats persists. The constraint's function has not atrophied; rather, its continued operation is essential for community resilience. The classification as a Rope reflects its ongoing, beneficial coordination function.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_reading_identity,
    'Is this constraint primarily about operational competence transmission, or is it better understood through a sibling reading of the ''catastrophe_memory_transmission'' kernel?',
    'Empirical studies of ritual efficacy: if operational outcomes (e.g., disaster preparedness, resource management) are demonstrably improved by ritual participation, this reading is strengthened. If symbolic cohesion or identity maintenance are the primary outcomes, sibling readings gain salience.',
    'If a sibling reading (e.g., ''symbol_continuity_reading'') is found to be more structurally accurate, the constraint''s classification might shift towards one emphasizing identity coordination or even extraction if symbolic fidelity is coercively enforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'This constraint is one reading of the ''catastrophe_memory_transmission'' kernel, focusing on operational competence.').

omega_variable(
    operational_vs_symbolic_primacy,
    'To what extent is the operational competence separable from the symbolic form of the ritual? Can the competence be transmitted effectively without the specific ritualistic elements?',
    'Comparative studies of communities that have abandoned traditional rituals but maintained explicit training for similar threats. If competence degrades, the ritual''s embeddedness is crucial. If competence is maintained, the operational aspect is more separable.',
    'If the operational competence is highly separable, the ritual''s ''rope'' classification might be challenged, potentially revealing a ''piton'' (if the operational function atrophies) or a ''snare'' (if symbolic enforcement becomes extractive without functional yield). If inseparable, the ''rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(operational_vs_symbolic_primacy, empirical, 'Ambiguity regarding the structural relationship between operational competence and symbolic ritual form.').

omega_variable(
    rope_or_mountain_for_competence_transmission,
    'Is the principle of transmitting survival competence through embodied patterns a universal, natural law (Mountain), or is the ritual itself a chosen, contingent coordination mechanism (Rope)?',
    'Cross-cultural and evolutionary studies: if all complex adaptive systems universally develop similar embodied transmission mechanisms for survival competence, it leans towards Mountain. If the specific ritual forms are highly contingent and culturally constructed, it reinforces Rope.',
    'If reclassified as a Mountain, the ''emerges_naturally'' flag would be set to true, and the extractiveness would be interpreted as an irreducible cost of a natural process. If it remains a Rope, the focus stays on the human-designed coordination function.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(rope_or_mountain_for_competence_transmission, conceptual, 'Ambiguity between the ritual as a contingent coordination (Rope) and the underlying principle as a universal law (Mountain).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_transmission__operational_competence_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_transmission__operational_competence_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 0, 0.15).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 20, 0.14).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 40, 0.15).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 60, 0.16).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 80, 0.15).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_transmission__operational_competence_reading, base_extractiveness, 100, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 20, 0.2).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 40, 0.2).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 60, 0.2).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 80, 0.2).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_transmission__operational_competence_reading, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
