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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
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
 *   constraint_id: catastrophe_memory_kernel__trauma_encoding_reading
 *   human_readable: Ritualized Intergenerational Trauma Encoding
 *   domain: religious_studies/collective_memory/ritual_practice
 *
 * SUMMARY:
 *   This constraint describes ritual practice as a mechanism for encoding and
 *   transmitting intergenerational trauma, intended to function as a warning
 *   system against future catastrophes. It is one reading of the
 *   'catastrophe_memory_kernel'. The constraint is claimed as a Tangled Rope,
 *   reflecting its dual function of coordinating collective memory and
 *   vigilance while imposing significant psychological costs on descendants.
 *   The metrics reflect a moderate-to-high extractiveness and suppression, as
 *   the ritual's persistence relies on social enforcement and the
 *   psychological burden it places on participants.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(catastrophe_memory_kernel__trauma_encoding_reading, 0.65).
domain_priors:suppression_score(catastrophe_memory_kernel__trauma_encoding_reading, 0.7).
domain_priors:theater_ratio(catastrophe_memory_kernel__trauma_encoding_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(catastrophe_memory_kernel__trauma_encoding_reading, resistance, 0.45).

% --- Constraint claim ---
narrative_ontology:constraint_claim(catastrophe_memory_kernel__trauma_encoding_reading, tangled_rope).
narrative_ontology:human_readable(catastrophe_memory_kernel__trauma_encoding_reading, "Ritualized Intergenerational Trauma Encoding").
narrative_ontology:topic_domain(catastrophe_memory_kernel__trauma_encoding_reading, "religious_studies/collective_memory/ritual_practice").

domain_priors:requires_active_enforcement(catastrophe_memory_kernel__trauma_encoding_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(catastrophe_memory_kernel__trauma_encoding_reading, 'bb01add4-ab19-42e5-8c06-38ed1fc7af03').
narrative_ontology:cs_kernel_codification('bb01add4-ab19-42e5-8c06-38ed1fc7af03', formalized).
narrative_ontology:cs_authority_grounding('bb01add4-ab19-42e5-8c06-38ed1fc7af03', lineage).
narrative_ontology:cs_interpretation_layer_present('bb01add4-ab19-42e5-8c06-38ed1fc7af03').
narrative_ontology:cs_reading_relation('bb01add4-ab19-42e5-8c06-38ed1fc7af03', catastrophe_memory_kernel__boundary_maintenance_reading, coexists_with).
narrative_ontology:cs_reading_relation('bb01add4-ab19-42e5-8c06-38ed1fc7af03', catastrophe_memory_kernel__survival_competence_reading, influences).
narrative_ontology:cs_reading_relation('bb01add4-ab19-42e5-8c06-38ed1fc7af03', catastrophe_memory_kernel__symbol_continuity_reading, coexists_with).
narrative_ontology:cs_axiom('bb01add4-ab19-42e5-8c06-38ed1fc7af03', foundational, trauma_as_preventative_wisdom).
narrative_ontology:cs_axiom_status(trauma_as_preventative_wisdom, holdable).
narrative_ontology:cs_axiom_grounding('bb01add4-ab19-42e5-8c06-38ed1fc7af03', trauma_as_preventative_wisdom, empirically_contingent).
narrative_ontology:cs_axiom('bb01add4-ab19-42e5-8c06-38ed1fc7af03', secondary, collective_memory_requires_visceral_transmission).
narrative_ontology:cs_axiom_status(collective_memory_requires_visceral_transmission, holdable).
narrative_ontology:cs_axiom_grounding('bb01add4-ab19-42e5-8c06-38ed1fc7af03', collective_memory_requires_visceral_transmission, conventional).
narrative_ontology:cs_reference_frame('bb01add4-ab19-42e5-8c06-38ed1fc7af03', catastrophe_prevention_through_memory).
narrative_ontology:cs_drift_state('bb01add4-ab19-42e5-8c06-38ed1fc7af03', contemporary_psychological_understanding, gap(axiom_overriding, substantial, false)).
narrative_ontology:cs_created_at('bb01add4-ab19-42e5-8c06-38ed1fc7af03', '').
narrative_ontology:cs_kernel_id(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(catastrophe_memory_kernel__trauma_encoding_reading, future_generations).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, descendants_bearing_psychological_burden).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(catastrophe_memory_kernel__trauma_encoding_reading, community_members_seeking_healing).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__trauma_encoding_reading, collective_survival_imperative).
narrative_ontology:constraint_vindicates(catastrophe_memory_kernel__trauma_encoding_reading, threat_vigilance_capacity).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The original generation or foundational figures who established the rituals, believing them essential for transmitting warnings and ensuring the community's long-term survival after a catastrophe. Their commitment is deeply embedded in the community's identity.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, ancestral_community_founders, agenda_setter,
    institutional, civilizational, identity_locked, global).

% Individuals in subsequent generations who experience the psychological and emotional costs of reliving historical trauma through prescribed ritual practices. They are bound by social and familial expectations, with limited avenues to opt out without risking ostracization.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, descendants_bearing_psychological_burden, payer,
    powerless, biographical, trapped, local).

% Administer and interpret the rituals, ensuring their continuity and fidelity to tradition. They genuinely believe in the efficacy of the rituals as a warning system and benefit from the social cohesion and authority derived from their role as custodians of collective memory.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, community_elders_and_ritual_leaders, agenda_setter,
    organized, generational, constrained, regional).

% The intended recipients of the warning system, benefiting from the collective vigilance and preparedness for potential future catastrophes. However, this benefit comes at the cost of inheriting the psychological burden encoded in the rituals.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, future_generations, beneficiary,
    moderate, civilizational, identity_locked, global).

% Participate in the rituals, seeking communal solidarity, identity affirmation, and a sense of continuity. While they may find some healing, they are still subject to the re-traumatizing aspects of the trauma encoding, often without alternative therapeutic outlets.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, community_members_seeking_healing, payer,
    moderate, biographical, constrained, local).

% Academics and researchers who study the rituals, their historical context, and their psychological and social effects on the community. They provide an external, critical perspective, often highlighting the costs and benefits from a detached viewpoint.
narrative_ontology:constraint_stakeholder(catastrophe_memory_kernel__trauma_encoding_reading, external_observers_scholars, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(catastrophe_memory_kernel__trauma_encoding_reading, diffuse).
narrative_ontology:fixing_cost_class(catastrophe_memory_kernel__trauma_encoding_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates collective memory and threat-response by embedding historical trauma in ritual practice, aiming to prevent future catastrophes by maintaining vigilance.
% TRANSFER_FUNCTION: Transfers psychological burden, emotional distress, and a heightened sense of threat from past generations to future ones, in exchange for perceived collective safety and preparedness.
% ABSENT_VOICES: Psychological and trauma-informed care professionals, as well as community members advocating for alternative, less re-traumatizing forms of remembrance and healing. They would argue for methods that process trauma rather than perpetually re-enacting it.
% DISAPPEARANCE_RATIONALE: If the rituals and their trauma encoding vanished overnight, the community's collective identity, its primary mechanism for threat-detection, and its social cohesion would be profoundly altered. It could lead to a loss of historical memory, a different form of collective identity, or a re-evaluation of how future threats are perceived and managed.
% FOUNDING_PROBLEM: A catastrophic historical event threatened the community's survival, necessitating a robust and enduring mechanism to transmit the memory of the event and prevent its recurrence.
% FOUNDING_PROBLEM_CORROBORATION: Community elders and ritual leaders attest that the founding problem (the threat of catastrophe) is still live and the rituals are essential. External observers, such as psychologists and sociologists, corroborate the historical catastrophe but contest the ongoing necessity or efficacy of trauma encoding as the primary or sole response, suggesting the problem's *solution* has become a problem itself.
narrative_ontology:disappearance_verdict(catastrophe_memory_kernel__trauma_encoding_reading, world_rearranges).
narrative_ontology:founding_problem_status(catastrophe_memory_kernel__trauma_encoding_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(catastrophe_memory_kernel__trauma_encoding_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(catastrophe_memory_kernel__trauma_encoding_reading, 'none', 1).
narrative_ontology:epsilon_provenance(catastrophe_memory_kernel__trauma_encoding_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.65) is driven by the psychological burden and re-traumatization experienced by descendants, which is a direct cost of the ritual's operation. Suppression (0.70) is high due to the strong social and cultural pressure to participate in and maintain these rituals, with few acceptable alternatives for remembrance or healing. The theater ratio (0.40) indicates that while the rituals have genuine functional aspects (maintaining memory, fostering cohesion), a significant portion of their performance is about reinforcing the trauma encoding itself, rather than purely adaptive warning. The metrics show a gradual increase in extractiveness and suppression over time, suggesting that as the original catastrophe recedes, the ritual's function shifts more towards perpetuating the trauma as an end in itself, rather than a dynamic warning system.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the ancestral founders and current ritual leaders, the constraint is a vital Rope or Scaffold, ensuring collective survival and identity. They perceive the psychological burden as a necessary cost for vigilance. From the perspective of descendants bearing the psychological burden, the same constraint operates as a Snare or Tangled Rope, extracting emotional well-being for a contested benefit. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The ancestral founders and current ritual leaders are agenda-setters, benefiting from the perceived collective safety and social cohesion. Future generations are beneficiaries of the intended warning system, but also bear the costs. Descendants bearing the psychological burden are clear payers/victims, experiencing direct extraction. Community members seeking healing are also payers, as they participate in the re-traumatization. External observers provide an analytical perspective, neither directly benefiting nor paying.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (preventing future catastrophe) is contested. While the original threat may have been real, the persistence of trauma encoding as the primary solution, despite its psychological costs, suggests mandatrophy. The constraint continues to operate, but its original mandate may have atrophied, with the mechanism of trauma transmission becoming an end in itself, rather than a means to a live problem. The high extractiveness and suppression, coupled with contested founding problem status, point to a potential shift from a necessary coordination mechanism to an extractive one.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    trauma_transmission_efficacy,
    'Is the ritualized trauma encoding genuinely effective as a warning system for future generations, or does it primarily impose psychological burden without commensurate protective benefit?',
    'Longitudinal studies comparing communities with and without such rituals, assessing actual threat-response efficacy versus psychological well-being outcomes. Expert psychological evaluation of the impact of re-enacted trauma.',
    'If found ineffective or counterproductive as a warning system, the constraint''s extractiveness would be re-evaluated as higher, and its coordination function diminished, pushing classification closer to a Snare. If effective, the coordination function would be strengthened.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(trauma_transmission_efficacy, empirical, 'Assessing the functional efficacy of trauma encoding as a warning system.').

omega_variable(
    alternative_memory_systems,
    'Are there alternative, less psychologically burdensome methods for transmitting collective memory and fostering vigilance that could achieve similar or better outcomes?',
    'Comparative anthropological and psychological research on diverse cultural memory practices, and pilot programs for trauma-informed remembrance in affected communities.',
    'If viable alternatives exist, the suppression metric would be re-evaluated as higher (due to the suppression of alternatives), and the constraint''s justification as a necessary coordination mechanism would weaken, supporting a reclassification towards Snare.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(alternative_memory_systems, conceptual, 'Exploring less extractive alternatives for collective memory transmission.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression primarily structural (social pressure, lack of alternatives) or internalized (descendants'' identity fused with the trauma narrative, making exit unthinkable)?',
    'Qualitative sociological studies exploring individual experiences of dissent and attempts to disengage from ritual, alongside psychological assessments of identity formation within the community.',
    'If internalized suppression is a significant factor, the constraint''s effective suppression is higher than the structural measure suggests, as the psychological burden persists even if external pressures lessen. This would amplify the effective extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism in ritual adherence.').

omega_variable(
    kernel_reading_framing,
    'Is this constraint best understood as a ''trauma encoding'' mechanism, or do other readings of the ''catastrophe_memory_kernel'' (e.g., ''symbol continuity'', ''survival competence'', ''boundary maintenance'') offer a more accurate primary framing?',
    'Comparative analysis of the structural and functional impacts of each reading, assessing which best explains the observed extractiveness, suppression, and stakeholder dynamics. Consensus among interdisciplinary scholars.',
    'Adopting a different primary reading would shift the focus of analysis, potentially altering the perceived beneficiaries, victims, and the overall classification of the constraint. For example, a ''symbol continuity'' reading might emphasize less extraction and more coordination.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_framing, conceptual, 'Ambiguity in the primary framing of the catastrophe memory kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(catastrophe_memory_kernel__trauma_encoding_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(cata_tr_t0, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(cata_tr_t20, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 20, 0.35).
narrative_ontology:measurement(cata_tr_t40, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 40, 0.4).
narrative_ontology:measurement(cata_tr_t60, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(cata_tr_t80, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 80, 0.39).
narrative_ontology:measurement(cata_tr_t100, catastrophe_memory_kernel__trauma_encoding_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(cata_be_t0, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 0, 0.55).
narrative_ontology:measurement(cata_be_t20, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(cata_be_t40, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 40, 0.61).
narrative_ontology:measurement(cata_be_t60, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(cata_be_t80, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 80, 0.64).
narrative_ontology:measurement(cata_be_t100, catastrophe_memory_kernel__trauma_encoding_reading, base_extractiveness, 100, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(cata_su_t0, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(cata_su_t20, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(cata_su_t40, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(cata_su_t60, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(cata_su_t80, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 80, 0.69).
narrative_ontology:measurement(cata_su_t100, catastrophe_memory_kernel__trauma_encoding_reading, suppression_requirement, 100, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(catastrophe_memory_kernel__trauma_encoding_reading, identity_coordination).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__boundary_maintenance_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__survival_competence_reading).
narrative_ontology:affects_constraint(catastrophe_memory_kernel__trauma_encoding_reading, catastrophe_memory_kernel__symbol_continuity_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of four distinct readings of the 'catastrophe_memory_kernel', each with different structural properties and classifications. This reading focuses on the encoding and transmission of trauma as a warning system.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
