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
 *   constraint_id: quran_ontological_status__created_reading
 *   human_readable: Qur'an as Created Divine Speech (Makhlūq)
 *   domain: islamic_theology/philosophy_of_language/political_authority
 *
 * SUMMARY:
 *   This constraint represents the theological position that the Qur'an is
 *   created divine speech (makhlūq), a view that emphasizes God's absolute
 *   transcendence and allows for rational interpretation of revelation. It is
 *   a reading of the broader 'quran_ontological_status' kernel, which is
 *   contested by 'uncreated' and 'state_enforced_creation' readings. This
 *   reading, while promoting interpretive flexibility, still operates as a
 *   Tangled Rope by coordinating theological discourse while extracting
 *   authority and interpretive certainty from traditionalist and literalist
 *   groups whose positions are undermined by its premises.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__created_reading, 0.55).
domain_priors:suppression_score(quran_ontological_status__created_reading, 0.45).
domain_priors:theater_ratio(quran_ontological_status__created_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, resistance, 0.6).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__created_reading, tangled_rope).
narrative_ontology:human_readable(quran_ontological_status__created_reading, "Qur'an as Created Divine Speech (Makhlūq)").
narrative_ontology:topic_domain(quran_ontological_status__created_reading, "islamic_theology/philosophy_of_language/political_authority").

domain_priors:requires_active_enforcement(quran_ontological_status__created_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__created_reading, '88c2b252-46bb-46d9-97ab-d5bd266baacd').
narrative_ontology:cs_kernel_codification('88c2b252-46bb-46d9-97ab-d5bd266baacd', fixed_text).
narrative_ontology:cs_authority_grounding('88c2b252-46bb-46d9-97ab-d5bd266baacd', expertise).
narrative_ontology:cs_interpretation_layer_present('88c2b252-46bb-46d9-97ab-d5bd266baacd').
narrative_ontology:cs_reading_relation('88c2b252-46bb-46d9-97ab-d5bd266baacd', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('88c2b252-46bb-46d9-97ab-d5bd266baacd', quran_ontological_status__state_enforced_creation_reading, influences).
narrative_ontology:cs_axiom('88c2b252-46bb-46d9-97ab-d5bd266baacd', foundational, quran_is_makhluk).
narrative_ontology:cs_axiom_status(quran_is_makhluk, holdable).
narrative_ontology:cs_axiom_grounding('88c2b252-46bb-46d9-97ab-d5bd266baacd', quran_is_makhluk, deontological).
narrative_ontology:cs_axiom('88c2b252-46bb-46d9-97ab-d5bd266baacd', foundational, divine_essence_transcends_temporality).
narrative_ontology:cs_axiom_status(divine_essence_transcends_temporality, holdable).
narrative_ontology:cs_axiom_grounding('88c2b252-46bb-46d9-97ab-d5bd266baacd', divine_essence_transcends_temporality, deontological).
narrative_ontology:cs_reference_frame('88c2b252-46bb-46d9-97ab-d5bd266baacd', divine_transcendence_and_reason).
narrative_ontology:cs_drift_state('88c2b252-46bb-46d9-97ab-d5bd266baacd', contemporary_islamic_thought, gap(stable, minor, true)).
narrative_ontology:cs_created_at('88c2b252-46bb-46d9-97ab-d5bd266baacd', '').
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

% Advocate for this reading, gaining hermeneutic authority and intellectual flexibility. They shape theological discourse and interpret scripture through rational inquiry, preserving divine transcendence.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, rationalist_theologians, agenda_setter,
    powerful, generational, mobile, global).

% Utilize this reading to justify modern interpretations of Islamic law and ethics, adapting tradition to contemporary challenges. They benefit from the interpretive flexibility it offers.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, reform_movements, beneficiary,
    organized, biographical, mobile, global).

% Find common ground with this reading, as it allows for philosophical engagement with revelation without compromising God's absolute transcendence. They contribute to and benefit from the intellectual space it creates.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, philosophical_schools, beneficiary,
    organized, generational, mobile, global).

% Their authority often derives from a literalist, uncreated understanding of the Qur'an. This reading challenges their interpretive monopoly and reduces their influence, forcing them to defend their positions against rationalist critiques.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, traditionalist_jurists, payer,
    powerful, generational, constrained, global).

% Their identity and practice are often tied to an unmediated, literal understanding of divine speech. This reading introduces interpretive layers that can feel alienating or undermine their direct connection to revelation, making them targets of its interpretive shift.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, literalist_communities, payer,
    powerless, biographical, identity_locked, local).

% Analyze the theological and political implications of this debate without direct participation in its normative claims. They observe the shifts in authority and interpretation.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, secular_scholars, observer,
    analytical, biographical, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:fixing_cost_class(quran_ontological_status__created_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a theological framework for interpreting the Qur'an that allows for rational inquiry, philosophical engagement, and adaptation to changing contexts, by asserting its created nature and God's transcendence.
% TRANSFER_FUNCTION: Transfers hermeneutic authority from literalist textual interpretation to rational theological methods; transfers interpretive flexibility to scholars and reformers, while extracting interpretive certainty and traditional authority from literalist and traditionalist groups.
% ABSENT_VOICES: Literalists and traditionalists who believe in the uncreated nature of the Qur'an are often marginalized or excluded from the interpretive discourse dominated by this reading, as their foundational premises are challenged.
% DISAPPEARANCE_RATIONALE: If this reading vanished overnight, the theological landscape would fundamentally shift, empowering literalist and traditionalist readings and potentially undermining rationalist and reformist approaches to scripture. The intellectual space for reconciling reason and revelation would be severely constrained.
% FOUNDING_PROBLEM: To reconcile divine revelation with philosophical reason, preserve God's absolute transcendence from any temporal association, and provide a basis for dynamic interpretation of scripture in changing contexts.
% FOUNDING_PROBLEM_CORROBORATION: Philosophical and theological texts from various periods of Islamic history, as well as contemporary reformist discourse, corroborate the ongoing relevance of these problems. Independent historical analysis confirms the intellectual challenges this reading sought to address.
narrative_ontology:disappearance_verdict(quran_ontological_status__created_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__created_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__created_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(quran_ontological_status__created_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__created_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__created_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(quran_ontological_status__created_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(quran_ontological_status__created_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.55) stems from the shift in hermeneutic authority and the devaluation of literalist interpretations, which impacts traditionalist jurists and communities. Suppression (0.45) reflects the active intellectual and social effort required to establish and maintain this reading's interpretive dominance against entrenched traditional views. The theater ratio is low (0.10) as this is a genuine theological position, not primarily performative. The claimed type is Tangled Rope because it offers a coordination function (rational theological framework) but simultaneously extracts from identifiable groups through the same structure.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of its beneficiaries, this reading is a necessary intellectual liberation, a Rope that coordinates reason and revelation. From the perspective of its victims, it is an imposition that undermines established truth and authority, experienced as a Snare. The engine's computation of Tangled Rope reflects this hybrid nature.
 *
 * DIRECTIONALITY LOGIC:
 *   Rationalist theologians, reform movements, and philosophical schools are beneficiaries, gaining interpretive flexibility and authority (low directionality). Traditionalist jurists and literalist communities are targets, as their authority and interpretive framework are challenged and diminished (high directionality). Secular scholars are observers, analyzing the dynamics without direct participation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    political_instrumentalization_ambiguity,
    'To what extent is this theological reading a genuine intellectual development, and to what extent has it been instrumentalized for political power (e.g., during the Mihna, the state-enforced creation doctrine)?',
    'Historical analysis of the motivations and outcomes of its proponents, distinguishing theological arguments from political alliances and state coercion.',
    'If primarily instrumentalized, its effective extractiveness and suppression would be higher, pushing it closer to a Snare, especially when considering the ''state_enforced_creation_reading'' sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(political_instrumentalization_ambiguity, empirical, 'Distinguishing theological intent from political instrumentalization.').

omega_variable(
    interpretive_flexibility_vs_elite_capture,
    'Does this reading genuinely open up interpretive flexibility for a broader community, or does it primarily shift interpretive authority to a new intellectual elite (rationalist theologians and philosophers)?',
    'Sociological study of hermeneutic access and participation across different social strata over time, comparing the distribution of interpretive power before and after its ascendancy.',
    'If it primarily leads to elite capture of interpretation, its coordination function is weaker, and its extraction from broader communities (who lose direct access to meaning) is higher, reinforcing its Tangled Rope classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_flexibility_vs_elite_capture, empirical, 'Assessing the actual distribution of interpretive power.').

omega_variable(
    divine_transcendence_redefinition,
    'Does this reading truly preserve God''s absolute transcendence, or does it merely redefine transcendence in a way that is more amenable to philosophical categories, potentially altering the traditional understanding of divine attributes?',
    'Comparative theological analysis of different schools of thought on divine attributes and their implications for human-divine interaction, assessing the conceptual coherence and continuity with prior traditions.',
    'If it significantly redefines transcendence in a way that departs from core theological tenets, it could be seen as a more radical break, increasing resistance from traditionalists and potentially leading to a re-evaluation of its foundational axioms.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(divine_transcendence_redefinition, conceptual, 'Conceptual integrity of divine transcendence in this reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__created_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__created_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qura_tr_t20, quran_ontological_status__created_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(qura_tr_t40, quran_ontological_status__created_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(qura_tr_t60, quran_ontological_status__created_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(qura_tr_t80, quran_ontological_status__created_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(qura_tr_t100, quran_ontological_status__created_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__created_reading, base_extractiveness, 0, 0.6).
narrative_ontology:measurement(qura_be_t20, quran_ontological_status__created_reading, base_extractiveness, 20, 0.58).
narrative_ontology:measurement(qura_be_t40, quran_ontological_status__created_reading, base_extractiveness, 40, 0.56).
narrative_ontology:measurement(qura_be_t60, quran_ontological_status__created_reading, base_extractiveness, 60, 0.55).
narrative_ontology:measurement(qura_be_t80, quran_ontological_status__created_reading, base_extractiveness, 80, 0.55).
narrative_ontology:measurement(qura_be_t100, quran_ontological_status__created_reading, base_extractiveness, 100, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__created_reading, suppression_requirement, 0, 0.55).
narrative_ontology:measurement(qura_su_t20, quran_ontological_status__created_reading, suppression_requirement, 20, 0.5).
narrative_ontology:measurement(qura_su_t40, quran_ontological_status__created_reading, suppression_requirement, 40, 0.48).
narrative_ontology:measurement(qura_su_t60, quran_ontological_status__created_reading, suppression_requirement, 60, 0.46).
narrative_ontology:measurement(qura_su_t80, quran_ontological_status__created_reading, suppression_requirement, 80, 0.45).
narrative_ontology:measurement(qura_su_t100, quran_ontological_status__created_reading, suppression_requirement, 100, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
