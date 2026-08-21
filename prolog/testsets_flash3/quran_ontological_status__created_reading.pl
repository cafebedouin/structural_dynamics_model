% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__created_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__created_reading, []).

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
 *   constraint_id: quran_ontological_status__created_reading
 *   human_readable: Qur'an as Created Divine Speech (Makhlūq)
 *   domain: islamic_theology/philosophy_of_language/political_authority
 *
 * SUMMARY:
 *   This constraint represents the theological position that the Qur'an is
 *   created divine speech (makhlūq), meaning it is a temporal artifact,
 *   distinct from God's eternal essence. This reading preserves God's
 *   absolute transcendence and opens the door for rational and philosophical
 *   interpretation of the text. It stands in contrast to the traditionalist
 *   view of the Qur'an as uncreated and co-eternal with God. The
 *   classification as 'rope' reflects its function as a coordination
 *   mechanism for theological and philosophical discourse, with moderate
 *   extraction from those whose authority is diminished by this interpretive
 *   flexibility.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__created_reading, 0.25).
domain_priors:suppression_score(quran_ontological_status__created_reading, 0.35).
domain_priors:theater_ratio(quran_ontological_status__created_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__created_reading, rope).
narrative_ontology:human_readable(quran_ontological_status__created_reading, "Qur'an as Created Divine Speech (Makhlūq)").
narrative_ontology:topic_domain(quran_ontological_status__created_reading, "islamic_theology/philosophy_of_language/political_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__created_reading, '784106d2-d634-4ce1-877f-0740383d7bab').
narrative_ontology:cs_kernel_codification('784106d2-d634-4ce1-877f-0740383d7bab', fixed_text).
narrative_ontology:cs_authority_grounding('784106d2-d634-4ce1-877f-0740383d7bab', expertise).
narrative_ontology:cs_interpretation_layer_present('784106d2-d634-4ce1-877f-0740383d7bab').
narrative_ontology:cs_reading_relation('784106d2-d634-4ce1-877f-0740383d7bab', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('784106d2-d634-4ce1-877f-0740383d7bab', quran_ontological_status__state_enforced_creation_reading, influences).
narrative_ontology:cs_axiom('784106d2-d634-4ce1-877f-0740383d7bab', foundational, god_transcends_temporal_artifacts).
narrative_ontology:cs_axiom_status(god_transcends_temporal_artifacts, holdable).
narrative_ontology:cs_axiom_grounding('784106d2-d634-4ce1-877f-0740383d7bab', god_transcends_temporal_artifacts, deontological).
narrative_ontology:cs_axiom('784106d2-d634-4ce1-877f-0740383d7bab', foundational, revelation_is_intelligible_to_reason).
narrative_ontology:cs_axiom_status(revelation_is_intelligible_to_reason, holdable).
narrative_ontology:cs_axiom_grounding('784106d2-d634-4ce1-877f-0740383d7bab', revelation_is_intelligible_to_reason, empirically_contingent).
narrative_ontology:cs_reference_frame('784106d2-d634-4ce1-877f-0740383d7bab', rational_theological_inquiry).
narrative_ontology:cs_drift_state('784106d2-d634-4ce1-877f-0740383d7bab', contemporary_islamic_thought, gap(stable, minor, true)).
narrative_ontology:cs_created_at('784106d2-d634-4ce1-877f-0740383d7bab', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__created_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, reform_movements).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, philosophical_schools).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, traditionalist_jurists).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, literalist_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the interpretive flexibility and hermeneutic authority that comes with a created Qur'an, allowing for rational inquiry and philosophical integration without being bound by literalist interpretations. Their intellectual project is validated by this reading.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, rationalist_theologians, beneficiary,
    organized, generational, mobile, global).

% Find theological justification for reinterpreting Islamic law and tradition in light of modern challenges, as the text is seen as a human-accessible artifact rather than an immutable, uncreated divine utterance. This enables progressive readings.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, reform_movements, beneficiary,
    moderate, generational, mobile, regional).

% Integrate Islamic revelation with Greek philosophy and other rational traditions, as the created nature of the Qur'an allows for a distinction between divine essence and temporal manifestation, preserving God's absolute transcendence.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, philosophical_schools, beneficiary,
    organized, generational, mobile, global).

% Bear the cost of diminished authority, as their interpretive methods often rely on the uncreated, eternal nature of the Qur'an to assert the absolute and fixed nature of its legal pronouncements. This reading undermines their hermeneutic foundation.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, traditionalist_jurists, payer,
    powerful, generational, constrained, global).

% Experience a challenge to their core identity and worldview, which often depends on the Qur'an being the direct, unmediated, and uncreated word of God. This reading introduces a layer of human interpretation that they resist, feeling their direct connection to the divine is compromised.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, literalist_communities, payer,
    powerless, biographical, identity_locked, local).

% Observe the theological debate, sometimes leveraging one reading over another to consolidate power or legitimize their rule. Historically, some authorities have enforced the 'created' doctrine, while others have supported the 'uncreated' view.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, political_authorities, observer,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates theological discourse by establishing a framework where divine transcendence is preserved, and the Qur'an can be interpreted through rational inquiry, allowing for intellectual engagement across diverse philosophical traditions.
% TRANSFER_FUNCTION: Transfers hermeneutic authority from literalist, traditionalist interpretations to rationalist and philosophical approaches, enabling a more flexible understanding of the text.
% ABSENT_VOICES: Extremist literalist factions who reject any rational interpretation and insist on an unmediated, uncreated text would be excluded; their voices are often suppressed by the very intellectual and political structures that benefit from this reading.
% DISAPPEARANCE_RATIONALE: If the 'created' reading vanished, the theological landscape would revert to a more literalist, traditionalist dominance, potentially stifling reform movements and philosophical inquiry within Islamic thought. The intellectual and political alliances built around this reading would dissolve.
% FOUNDING_PROBLEM: To reconcile divine transcendence with the immanence of revelation, and to allow for rational interpretation of the Qur'an in response to philosophical challenges and the need for legal flexibility.
% FOUNDING_PROBLEM_CORROBORATION: Rationalist theologians and philosophical schools attest that the problem of reconciling faith and reason, and adapting religious law, remains live. Traditionalist jurists, while opposing the solution, acknowledge the historical existence of the problem that this reading sought to address.
narrative_ontology:disappearance_verdict(quran_ontological_status__created_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__created_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__created_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quran_ontological_status__created_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__created_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__created_reading_tests).
:- end_tests(quran_ontological_status__created_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.25) because while this reading empowers certain intellectual factions, it primarily functions as a framework for theological coordination rather than direct material extraction. Suppression (0.35) is also moderate, as this reading often requires intellectual and sometimes political effort to counter literalist or traditionalist opposition, but it is not inherently coercive. Theater ratio is low (0.1) as the theological arguments are genuine and functional. The temporal measurements show relative stability, reflecting the enduring nature of this theological debate.
 *
 * PERSPECTIVAL GAP:
 *   The beneficiaries perceive this as a liberating and intellectually robust framework for understanding revelation, a true 'rope' of coordination. The payers, however, experience it as an erosion of divine authority and a threat to the stability of religious law, perceiving it as more extractive due to the loss of their interpretive monopoly.
 *
 * DIRECTIONALITY LOGIC:
 *   Rationalist theologians, reform movements, and philosophical schools are beneficiaries (low d) as this reading provides the intellectual space and authority for their work. Traditionalist jurists and literalist communities are payers (high d) as their authority and worldview are challenged by the interpretive flexibility implied by a created Qur'an. Political authorities are observers, sometimes aligning with one side for their own ends.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_vs_political_enforcement,
    'Is the persistence of the ''created'' reading due to its theological coherence and intellectual appeal, or to historical periods of state enforcement (e.g., the Mihna)?',
    'Historical analysis of periods where the doctrine was not state-enforced, examining its intellectual vitality and influence during those times.',
    'If primarily due to state enforcement, the ''created'' reading''s classification might shift towards ''tangled_rope'' or ''snare'' during those periods, reflecting coercive extraction. If due to intellectual appeal, its ''rope'' classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theological_vs_political_enforcement, empirical, 'Distinguishing between intellectual and coercive drivers of the reading''s prevalence.').

omega_variable(
    interpretive_flexibility_vs_textual_authority,
    'Does the ''created'' reading''s emphasis on interpretive flexibility undermine the perceived divine authority of the Qur''an for some communities, leading to a sense of loss or alienation?',
    'Sociological studies and qualitative interviews with literalist communities to assess their subjective experience of textual authority under this reading.',
    'If it significantly erodes perceived divine authority for a large segment of the population, the ''extraction'' metric for those communities would be higher, potentially pushing their seat classification towards ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_flexibility_vs_textual_authority, empirical, 'Assessing the impact of interpretive flexibility on the perceived authority of the text.').

omega_variable(
    kernel_framing_ambiguity,
    'Is the ''quran_ontological_status'' kernel best framed as a theological dispute over divine attributes, or as a political contest over interpretive authority?',
    'Analysis of the historical context of the Mihna and subsequent periods, focusing on the motivations of key actors and the consequences for political power structures.',
    'If primarily political, the ''extraction'' and ''suppression'' metrics for all readings would be re-evaluated upwards, and the ''claimed_type'' for all readings might shift towards ''tangled_rope'' or ''snare'', reflecting the underlying power struggle.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_framing_ambiguity, conceptual, 'Ambiguity in the primary framing of the kernel: theological vs. political.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__created_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__created_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qura_tr_t300, quran_ontological_status__created_reading, theater_ratio, 300, 0.1).
narrative_ontology:measurement(qura_tr_t600, quran_ontological_status__created_reading, theater_ratio, 600, 0.1).
narrative_ontology:measurement(qura_tr_t900, quran_ontological_status__created_reading, theater_ratio, 900, 0.1).
narrative_ontology:measurement(qura_tr_t1200, quran_ontological_status__created_reading, theater_ratio, 1200, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__created_reading, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(qura_be_t300, quran_ontological_status__created_reading, base_extractiveness, 300, 0.25).
narrative_ontology:measurement(qura_be_t600, quran_ontological_status__created_reading, base_extractiveness, 600, 0.22).
narrative_ontology:measurement(qura_be_t900, quran_ontological_status__created_reading, base_extractiveness, 900, 0.23).
narrative_ontology:measurement(qura_be_t1200, quran_ontological_status__created_reading, base_extractiveness, 1200, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__created_reading, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(qura_su_t300, quran_ontological_status__created_reading, suppression_requirement, 300, 0.35).
narrative_ontology:measurement(qura_su_t600, quran_ontological_status__created_reading, suppression_requirement, 600, 0.32).
narrative_ontology:measurement(qura_su_t900, quran_ontological_status__created_reading, suppression_requirement, 900, 0.33).
narrative_ontology:measurement(qura_su_t1200, quran_ontological_status__created_reading, suppression_requirement, 1200, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__created_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'quran_ontological_status' kernel, which also includes 'uncreated_reading' and 'state_enforced_creation_reading'. Each reading represents a distinct structural claim about the Qur'an's nature and its implications for authority and interpretation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
