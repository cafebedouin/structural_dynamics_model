% ============================================================================
% CONSTRAINT STORY: catastrophe_memory_kernel__trauma_encoding_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_catastrophe_memory_kernel__trauma_encoding_reading, []).

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
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: catastrophe_memory_kernel__trauma_encoding_reading
 *   human_readable: Ritual as Intergenerational Trauma Encoding for Threat Vigilance
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint models a specific reading of the
 *   'catastrophe_memory_kernel' where ritual functions primarily as a
 *   mechanism for encoding and transmitting intergenerational trauma to
 *   maintain collective threat vigilance. The ritual, originating from a
 *   founding generation's direct experience of catastrophe, imposes a
 *   psychological burden on descendants (victims) while aiming to enhance
 *   collective preparedness (beneficiary). This reading emphasizes the cost
 *   of memory transmission. The claimed type is 'tangled_rope' because it
 *   genuinely coordinates collective memory but does so with significant,
 *   asymmetric extraction of psychological well-being.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, 0.68).
domain_priors:suppression_score(catastrophe_memory_kernel__trauma_encoding_reading, 0.75).
domain_priors:theater_ratio(catastrophe_memory_kernel__trauma_encoding_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, extractiveness, 0.68).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__trauma_encoding_reading, "Ritual as Intergenerational Trauma Encoding for Threat Vigilance").
narrative_ontology:topic_domain(catastrophe_memory_kernel__trauma_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__trauma_encoding_reading, '58f37765-1da2-4ae6-8fe8-60a2d3889124').
narrative_ontology:cs_kernel_codification('58f37765-1da2-4ae6-8fe8-60a2d3889124', implicit).
narrative_ontology:cs_authority_grounding('58f37765-1da2-4ae6-8fe8-60a2d3889124', practice).
narrative_ontology:cs_interpretation_layer_present('58f37765-1da2-4ae6-8fe8-60a2d3889124').
narrative_ontology:cs_reading_relation('58f37765-1da2-4ae6-8fe8-60a2d3889124', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_reading_relation('58f37765-1da2-4ae6-8fe8-60a2d3889124', catastrophe_memory_kernel__survival_competence_reading, coexists_with).
narrative_ontology:cs_reading_relation('58f37765-1da2-4ae6-8fe8-60a2d3889124', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_axiom('58f37765-1da2-4ae6-8fe8-60a2d3889124', foundational, trauma_as_essential_warning).
narrative_ontology:cs_axiom_status(trauma_as_essential_warning, holdable).
narrative_ontology:cs_axiom_grounding('58f37765-1da2-4ae6-8fe8-60a2d3889124', trauma_as_essential_warning, empirically_contingent).
narrative_ontology:cs_axiom('58f37765-1da2-4ae6-8fe8-60a2d3889124', secondary, memory_transmission_requires_affective_intensity).
narrative_ontology:cs_axiom_status(memory_transmission_requires_affective_intensity, holdable).
narrative_ontology:cs_axiom_grounding('58f37765-1da2-4ae6-8fe8-60a2d3889124', memory_transmission_requires_affective_intensity, empirically_contingent).
narrative_ontology:cs_reference_frame('58f37765-1da2-4ae6-8fe8-60a2d3889124', perpetual_vigilance_through_suffering).
narrative_ontology:cs_drift_state('58f37765-1da2-4ae6-8fe8-60a2d3889124', contemporary_psychological_awareness, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('58f37765-1da2-4ae6-8fe8-60a2d3889124', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, future_generations_collective_vigilance).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, descendants_with_psychological_burden).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The initial generation that experienced the catastrophe and established the ritual practices to ensure its memory and warning are transmitted. They are driven by a profound need to prevent recurrence and ensure survival, embedding their trauma into the collective memory system.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, founding_generation_survivors, agenda_setter,
    institutional, generational, identity_locked, local).

% Later generations who inherit the ritual and, through it, the psychological burden of the ancestral trauma. They experience heightened anxiety, hyper-vigilance, and a sense of impending doom, even in the absence of immediate threat. Their identity is deeply intertwined with the collective memory, making disengagement difficult.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, descendants_with_psychological_burden, payer,
    moderate, biographical, identity_locked, local).

% The collective entity of future generations benefits from an enhanced threat-detection and response capacity, theoretically reducing vulnerability to similar catastrophes. This benefit comes at the cost of individual psychological well-being for its members.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, future_generations_collective_vigilance, beneficiary,
    organized, generational, constrained, local).

% Those responsible for maintaining and transmitting the ritual practices across generations. They enforce adherence to the ritual, believing it essential for group survival and the preservation of collective memory. They may not fully perceive the psychological cost to individuals.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, ritual_leaders_and_educators, agenda_setter,
    organized, biographical, constrained, local).

% Academics and clinicians who study the effects of intergenerational trauma and ritual. They analyze the psychological costs and benefits, often advocating for interventions that mitigate the burden while preserving adaptive memory.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, external_observers_psychologists, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and threat vigilance across generations, ensuring that the lessons and emotional impact of a past catastrophe are not forgotten, thereby fostering group cohesion and preparedness.
% TRANSFER_FUNCTION: Transfers a psychological burden (trauma, anxiety, hyper-vigilance) from the founding generation to descendants, in exchange for a perceived benefit of collective threat-detection and survival capacity.
% ABSENT_VOICES: Descendants who wish to process and integrate the trauma rather than perpetually re-enact it, or those who question the efficacy of the ritual as a warning system versus its psychological cost. Their voices are often suppressed by the collective imperative for vigilance and loyalty to ancestral suffering.
% DISAPPEARANCE_RATIONALE: If the ritual vanished, the collective memory of the catastrophe would fade, and with it, the specific form of intergenerational trauma. While this might alleviate psychological burden, it could also lead to a loss of collective identity and a perceived reduction in threat preparedness, forcing the community to find new ways to transmit history and foster cohesion.
% FOUNDING_PROBLEM: To prevent the recurrence of a catastrophic event by ensuring its memory and the associated threat signals are indelibly etched into the collective consciousness of future generations.
% FOUNDING_PROBLEM_CORROBORATION: The founding generation's testimony and historical records attest to the original catastrophe. Ritual leaders and many community members attest the problem is still live, citing ongoing threats and the need for vigilance. External psychologists acknowledge the historical trauma but question the efficacy and cost-benefit of its perpetual re-enactment as a 'live' problem.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__trauma_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__trauma_encoding_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__trauma_encoding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(catastrophe_memory_kernel__trauma_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 0.68, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(catastrophe_memory_kernel__trauma_encoding_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is high (0.68) because the psychological burden on descendants is substantial and persistent, often manifesting as anxiety and hyper-vigilance. Suppression (0.75) is also high, as the collective identity and social cohesion are deeply tied to the ritual, making it difficult for individuals to disengage or challenge its traumatic aspects without risking ostracization or a sense of betrayal. Theater ratio is low (0.20) because the ritual is genuinely functional in transmitting memory and vigilance, even if its costs are high. Accessibility collapse is moderate (0.60) as alternatives for processing trauma or transmitting history exist, but are difficult to access or legitimize within the community. Resistance is moderate (0.45) as some individuals and external observers question the cost-benefit, but direct challenge is rare due to identity-lock and social pressure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the founding generation and ritual leaders, the constraint is a necessary, albeit difficult, coordination mechanism for survival. From the perspective of individual descendants, it is a source of inherited suffering and a constraint on their psychological autonomy. External observers may see it as a tangled rope, recognizing both the coordination function and the extractive psychological cost.
 *
 * DIRECTIONALITY LOGIC:
 *   The founding generation and ritual leaders act as agenda-setters, benefiting from the perpetuation of the warning system. Future generations, as a collective, are the intended beneficiaries of enhanced vigilance. However, individual descendants bear the psychological cost, making them victims. Their identity is locked into the collective memory, making exit difficult and amplifying the effective extraction.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate (preventing recurrence of catastrophe) is still considered 'live' by many, preventing a clear mandatrophy resolution. However, the 'contested' status of the founding problem suggests that while the problem exists, the ritual's efficacy or necessity in its current form is debated. This prevents mislabeling it as a pure snare, as a genuine coordination function (collective memory/vigilance) is present, but the high extraction and suppression indicate it's far from a pure rope.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (social pressure, identity-lock) or internalized (descendants'' belief in the necessity of their burden)?',
    'Post-exit suppression trajectory: if psychological burden and hyper-vigilance persist after disengagement from the ritual, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making true liberation more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in intergenerational trauma.').

omega_variable(
    efficacy_of_trauma_as_warning,
    'Does the encoding of trauma genuinely enhance threat vigilance and survival, or does it primarily cause psychological distress without proportional adaptive benefit?',
    'Longitudinal studies comparing communities with different trauma transmission patterns, or psychological interventions designed to mitigate burden while preserving adaptive memory.',
    'If the adaptive benefit is low, the constraint shifts closer to a snare, as the coordination story (warning system) becomes more of a cover for pure extraction (psychological burden). If high, it reinforces the tangled rope classification.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(efficacy_of_trauma_as_warning, empirical, 'The actual adaptive efficacy of trauma encoding as a warning system.').

omega_variable(
    kernel_reading_focus,
    'Is this constraint primarily about trauma encoding, or is it better understood through a sibling reading like symbol continuity or survival competence?',
    'Analysis of community discourse, ritual emphasis, and psychological impact. If the primary concern shifts to identity or adaptive skills, a different reading would be more appropriate.',
    'Reclassifying to a different reading (e.g., ''symbol_continuity_reading'') would likely alter the extractiveness and beneficiary/victim structure, as the core function and its costs would be reframed.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_focus, conceptual, 'This constraint is one reading of the ''catastrophe_memory_kernel'', focusing on trauma encoding. Sibling readings include ''symbol_continuity_reading'', ''survival_competence_reading'', and ''boundary_maintenance_reading''. Each emphasizes different aspects of the ritual''s function and impact.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__trauma_encoding_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 20, 0.18).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 60, 0.2).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 80, 0.19).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 100, 0.2).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 20, 0.63).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 40, 0.66).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 60, 0.68).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 80, 0.69).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 100, 0.68).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0, 0.7).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 20, 0.72).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 40, 0.74).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 60, 0.75).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 80, 0.75).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__trauma_encoding_reading, attachment_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__symbol_continuity_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__boundary_maintenance_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of several readings of the 'catastrophe_memory_kernel', each focusing on a different primary function and structural impact of collective memory rituals. This reading emphasizes trauma encoding for threat vigilance.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
