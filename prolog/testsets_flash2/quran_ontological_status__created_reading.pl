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
 *   created divine speech (makhlūq), distinct from God's eternal essence.
 *   This reading emphasizes divine transcendence and allows for rational and
 *   contextual interpretation of the text. It is a 'rope' because it
 *   facilitates coordination among rationalist theologians and reform
 *   movements, providing a framework for intellectual engagement with
 *   revelation. The metrics reflect a relatively low extractiveness and
 *   suppression, as this reading primarily offers an interpretive lens rather
 *   than imposing coercive structures, though it does challenge the authority
 *   of traditionalist and literalist groups.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__created_reading, 0.3).
domain_priors:suppression_score(quran_ontological_status__created_reading, 0.2).
domain_priors:theater_ratio(quran_ontological_status__created_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, resistance, 0.3).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__created_reading, rope).
narrative_ontology:human_readable(quran_ontological_status__created_reading, "Qur'an as Created Divine Speech (Makhlūq)").
narrative_ontology:topic_domain(quran_ontological_status__created_reading, "islamic_theology/philosophy_of_language/political_authority").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__created_reading, '86015da9-44af-42d5-a2f4-d9caf881b316').
narrative_ontology:cs_kernel_codification('86015da9-44af-42d5-a2f4-d9caf881b316', formalized).
narrative_ontology:cs_authority_grounding('86015da9-44af-42d5-a2f4-d9caf881b316', expertise).
narrative_ontology:cs_interpretation_layer_present('86015da9-44af-42d5-a2f4-d9caf881b316').
narrative_ontology:cs_reading_relation('86015da9-44af-42d5-a2f4-d9caf881b316', quran_ontological_status__uncreated_reading, coexists_with).
narrative_ontology:cs_reading_relation('86015da9-44af-42d5-a2f4-d9caf881b316', quran_ontological_status__state_enforced_creation_reading, influences).
narrative_ontology:cs_axiom('86015da9-44af-42d5-a2f4-d9caf881b316', foundational, gods_essence_transcends_temporal_artifacts).
narrative_ontology:cs_axiom_status(gods_essence_transcends_temporal_artifacts, holdable).
narrative_ontology:cs_axiom_grounding('86015da9-44af-42d5-a2f4-d9caf881b316', gods_essence_transcends_temporal_artifacts, deontological).
narrative_ontology:cs_axiom('86015da9-44af-42d5-a2f4-d9caf881b316', foundational, revelation_is_amenable_to_rational_inquiry).
narrative_ontology:cs_axiom_status(revelation_is_amenable_to_rational_inquiry, holdable).
narrative_ontology:cs_axiom_grounding('86015da9-44af-42d5-a2f4-d9caf881b316', revelation_is_amenable_to_rational_inquiry, deontological).
narrative_ontology:cs_reference_frame('86015da9-44af-42d5-a2f4-d9caf881b316', rational_theological_inquiry).
narrative_ontology:cs_drift_state('86015da9-44af-42d5-a2f4-d9caf881b316', contemporary_islamic_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('86015da9-44af-42d5-a2f4-d9caf881b316', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__created_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, reform_movements).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, philosophical_schools).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, traditionalist_jurists).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, literalist_communities).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, reform_members).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the interpretive flexibility and hermeneutic authority that arises when the Qur'an is understood as a created artifact, allowing for rational inquiry and contextual interpretation without compromising divine transcendence.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, rationalist_theologians, beneficiary,
    organized, generational, mobile, global).

% Find theological grounding for modern interpretations and social reforms, as a created Qur'an allows for re-evaluation of historical interpretations in light of contemporary ethical and social challenges.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, reform_members, beneficiary,
    moderate, biographical, constrained, regional).

% Gain a framework for reconciling revelation with philosophical reason, as a created text can be understood through human categories and logic without diminishing God's absolute otherness.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, philosophical_schools, beneficiary,
    organized, generational, mobile, global).

% Experience a challenge to their authority, which often derives from the perceived eternal and unmediated nature of the Qur'an. The created reading introduces interpretive layers that can undermine their established legal methodologies.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, traditionalist_jurists, payer,
    powerful, generational, constrained, global).

% Their identity and worldview are often deeply intertwined with the belief in the Qur'an as uncreated, direct divine speech. The created reading can be perceived as a threat to the sanctity and literal truth of the text, causing existential distress and resistance.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, literalist_communities, payer,
    moderate, biographical, identity_locked, local).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates theological discourse by establishing a framework where divine transcendence is preserved, and revelation is amenable to rational inquiry and contextual interpretation, fostering intellectual engagement within Islamic thought.
% TRANSFER_FUNCTION: Transfers hermeneutic authority from literalist, traditionalist interpretations to rationalist and philosophical approaches, enabling a more flexible understanding of textual meaning.
% ABSENT_VOICES: Extremist literalist factions who reject any form of rational interpretation or contextualization of the Qur'an are often excluded from mainstream theological discourse, as their views are seen as incompatible with intellectual engagement.
% DISAPPEARANCE_RATIONALE: If the created reading of the Qur'an's ontological status vanished, the theological landscape would revert to a more literalist and traditionalist understanding, potentially stifling rational inquiry and reform movements. The relationship between divine transcendence and revelation would be re-negotiated, likely leading to renewed theological conflicts.
% FOUNDING_PROBLEM: Theological problem of reconciling God's absolute transcendence (tanzīh) with the immanence of revelation, and the need for rational inquiry into religious texts without compromising divine unity (tawhīd).
% FOUNDING_PROBLEM_CORROBORATION: Philosophical theologians and modern Islamic scholars continue to attest to the live nature of this problem, emphasizing the ongoing need to harmonize reason and revelation. Traditionalist scholars, while disagreeing with the solution, acknowledge the historical existence of the theological tension.
narrative_ontology:disappearance_verdict(quran_ontological_status__created_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__created_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__created_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quran_ontological_status__created_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__created_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is low (0.3) because this reading primarily reallocates interpretive authority rather than imposing direct material costs. Suppression is also low (0.2) as it relies on theological argument and intellectual persuasion rather than active enforcement, though it does suppress literalist interpretations. The theater ratio is minimal (0.1) as the theological arguments are generally sincere. The historical measurements show some fluctuation, reflecting periods of greater or lesser intellectual dominance of this theological position.
 *
 * PERSPECTIVAL GAP:
 *   Rationalist theologians and reform movements experience this as a liberating framework (beneficiary seat), enabling deeper engagement with revelation. Traditionalist jurists and literalist communities, however, perceive it as an erosion of divine authority and a threat to their established interpretive methods (payer seat). The engine will compute these divergent classifications based on the declared structural relationships.
 *
 * DIRECTIONALITY LOGIC:
 *   Rationalist theologians, reform movements, and philosophical schools are beneficiaries (low d) as this reading grants them greater hermeneutic authority and intellectual freedom. Traditionalist jurists and literalist communities are victims/payers (higher d) as their authority and identity are challenged by this interpretive framework. The constraint subsidizes intellectual flexibility and extracts from textual fixity.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling a theological framework as pure extraction. While it does challenge existing authorities, its primary function is to coordinate a specific mode of theological inquiry and reconcile perceived tensions between divine transcendence and revelation. It is not a 'snare' because its persistence depends on intellectual coherence and persuasion, not coercion, and it offers genuine benefits to its adherents.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_coercion_potential,
    'Does the ''created'' reading, when adopted by state power, become a tool for political coercion, as seen in historical inquisitions (mihna)?',
    'Historical analysis of state-sponsored theological enforcement and contemporary examination of political uses of theological doctrines.',
    'If the reading is found to be readily co-opted for state coercion, its effective extractiveness and suppression would be significantly higher, potentially reclassifying it as a ''tangled_rope'' or ''snare'' when linked to political authority.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(theological_coercion_potential, empirical, 'Ambiguity of the ''created'' reading''s potential for political instrumentalization.').

omega_variable(
    interpretive_flexibility_limits,
    'To what extent does the ''created'' reading genuinely allow for interpretive flexibility without leading to arbitrary or relativistic interpretations of the Qur''an?',
    'Analysis of hermeneutic methodologies developed within this reading and their adherence to established linguistic and theological principles.',
    'If interpretive flexibility is found to be unbounded, it could undermine the constraint''s coordination function, leading to fragmentation rather than coherent theological discourse. If too constrained, it might not deliver the promised benefits to rationalist theologians.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_flexibility_limits, conceptual, 'The boundaries and implications of interpretive flexibility inherent in the ''created'' reading.').

omega_variable(
    divine_transcendence_vs_immanence,
    'Is the ''created'' reading the only theologically sound way to preserve divine transcendence, or do other readings (e.g., uncreated with nuanced interpretation) also achieve this goal?',
    'Comparative theological analysis of different schools of thought regarding divine attributes and the nature of revelation.',
    'If other readings are found to equally preserve transcendence, the unique ''beneficiary'' status of this reading for rationalist theologians might be diminished, affecting its perceived coordination value.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(divine_transcendence_vs_immanence, conceptual, 'The necessity of the ''created'' reading for preserving divine transcendence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__created_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__created_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qura_tr_t25, quran_ontological_status__created_reading, theater_ratio, 25, 0.08).
narrative_ontology:measurement(qura_tr_t50, quran_ontological_status__created_reading, theater_ratio, 50, 0.07).
narrative_ontology:measurement(qura_tr_t75, quran_ontological_status__created_reading, theater_ratio, 75, 0.09).
narrative_ontology:measurement(qura_tr_t100, quran_ontological_status__created_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__created_reading, base_extractiveness, 0, 0.3).
narrative_ontology:measurement(qura_be_t25, quran_ontological_status__created_reading, base_extractiveness, 25, 0.28).
narrative_ontology:measurement(qura_be_t50, quran_ontological_status__created_reading, base_extractiveness, 50, 0.25).
narrative_ontology:measurement(qura_be_t75, quran_ontological_status__created_reading, base_extractiveness, 75, 0.27).
narrative_ontology:measurement(qura_be_t100, quran_ontological_status__created_reading, base_extractiveness, 100, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__created_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(qura_su_t25, quran_ontological_status__created_reading, suppression_requirement, 25, 0.18).
narrative_ontology:measurement(qura_su_t50, quran_ontological_status__created_reading, suppression_requirement, 50, 0.15).
narrative_ontology:measurement(qura_su_t75, quran_ontological_status__created_reading, suppression_requirement, 75, 0.17).
narrative_ontology:measurement(qura_su_t100, quran_ontological_status__created_reading, suppression_requirement, 100, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__created_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__uncreated_reading).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'quran_ontological_status' kernel, focusing on the theological implications of the Qur'an as created divine speech. It is structurally distinct from the 'uncreated' reading and the 'state_enforced_creation' reading, which have different beneficiary/victim structures and classifications.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
