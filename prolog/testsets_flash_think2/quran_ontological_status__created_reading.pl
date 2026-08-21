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
 *   human_readable: Qur'an as Created Divine Speech (Rationalist Reading)
 *   domain: islamic_theology/philosophy_of_language/political_authority
 *
 * SUMMARY:
 *   This constraint represents the theological position that the Qur'an is
 *   created divine speech (makhlūq), a view historically associated with
 *   rationalist Islamic theology. It asserts God's absolute transcendence,
 *   distinguishing His eternal essence from all temporal artifacts, including
 *   revelation. This reading emphasizes the role of human reason in
 *   interpreting scripture and challenges literalist or anthropomorphic
 *   understandings. The 'claimed_type' of rope reflects its intended function
 *   as a coordination artifact for theological understanding, even though its
 *   historical enforcement and impact on traditional authorities suggest
 *   extractive dynamics.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__created_reading, 0.6).
domain_priors:suppression_score(quran_ontological_status__created_reading, 0.7).
domain_priors:theater_ratio(quran_ontological_status__created_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(quran_ontological_status__created_reading, resistance, 0.8).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__created_reading, rope).
narrative_ontology:human_readable(quran_ontological_status__created_reading, "Qur'an as Created Divine Speech (Rationalist Reading)").
narrative_ontology:topic_domain(quran_ontological_status__created_reading, "islamic_theology/philosophy_of_language/political_authority").

domain_priors:requires_active_enforcement(quran_ontological_status__created_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__created_reading, 'd04e45e0-7937-4a46-a655-2c8aabcccb71').
narrative_ontology:cs_kernel_codification('d04e45e0-7937-4a46-a655-2c8aabcccb71', formalized).
narrative_ontology:cs_authority_grounding('d04e45e0-7937-4a46-a655-2c8aabcccb71', expertise).
narrative_ontology:cs_interpretation_layer_present('d04e45e0-7937-4a46-a655-2c8aabcccb71').
narrative_ontology:cs_reading_relation('d04e45e0-7937-4a46-a655-2c8aabcccb71', quran_ontological_status__uncreated_reading, forecloses).
narrative_ontology:cs_reading_relation('d04e45e0-7937-4a46-a655-2c8aabcccb71', quran_ontological_status__state_enforced_creation_reading, influences).
narrative_ontology:cs_axiom('d04e45e0-7937-4a46-a655-2c8aabcccb71', foundational, divine_transcendence_absolute).
narrative_ontology:cs_axiom_status(divine_transcendence_absolute, holdable).
narrative_ontology:cs_axiom_grounding('d04e45e0-7937-4a46-a655-2c8aabcccb71', divine_transcendence_absolute, deontological).
narrative_ontology:cs_axiom('d04e45e0-7937-4a46-a655-2c8aabcccb71', foundational, reason_as_interpretive_tool).
narrative_ontology:cs_axiom_status(reason_as_interpretive_tool, holdable).
narrative_ontology:cs_axiom_grounding('d04e45e0-7937-4a46-a655-2c8aabcccb71', reason_as_interpretive_tool, conventional).
narrative_ontology:cs_reference_frame('d04e45e0-7937-4a46-a655-2c8aabcccb71', divine_transcendence_and_reason).
narrative_ontology:cs_drift_state('d04e45e0-7937-4a46-a655-2c8aabcccb71', post_mihna_era, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('d04e45e0-7937-4a46-a655-2c8aabcccb71', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__created_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, philosophical_schools).
narrative_ontology:constraint_beneficiary(quran_ontological_status__created_reading, reform_movements).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, traditionalist_jurists).
narrative_ontology:constraint_victim(quran_ontological_status__created_reading, literalist_communities).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Gain hermeneutic authority, allowing for allegorical and rational interpretation of scripture. Their intellectual methods are validated by this theological position.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, rationalist_theologians, beneficiary,
    institutional, generational, analytical, global).

% Their methods of inquiry are validated by a flexible, created text, allowing for integration of philosophical reasoning into theological discourse.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, philosophical_schools, beneficiary,
    organized, generational, analytical, global).

% Find theological justification for reinterpreting Islamic law and practice in modern contexts, moving away from rigid textual literalism and traditional authority.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, reform_movements, beneficiary,
    organized, biographical, mobile, regional).

% Their authority, often based on strict adherence to fixed textual interpretations and the uncreated nature of the Qur'an, is challenged and diminished by the flexibility introduced by the created Qur'an doctrine.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, traditionalist_jurists, payer,
    institutional, civilizational, constrained, global).

% Their identity and worldview are deeply tied to the belief in the Qur'an as unmediated, eternal divine speech. This reading is perceived as a direct threat to their foundational beliefs and the divine status of the text.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, literalist_communities, payer,
    organized, biographical, identity_locked, local).

% Historically, some political authorities (e.g., Abbasid Caliphate during the Mihna) adopted and enforced this doctrine to assert their own interpretive authority over religious scholars and consolidate power. Their role can shift between enforcing and suppressing, depending on their alignment.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, political_authorities, agenda_setter,
    institutional, biographical, constrained, national).

% Study the historical, theological, and political implications of this doctrine without taking a confessional stance, analyzing its structural effects on Islamic thought and society.
narrative_ontology:constraint_stakeholder(quran_ontological_status__created_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__created_reading, rationalist_theologians).
narrative_ontology:fixing_cost_class(quran_ontological_status__created_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates theological understanding by asserting divine transcendence and allowing for rational interpretation of revelation, preventing anthropomorphism and textual rigidity in Islamic thought.
% TRANSFER_FUNCTION: Transfers interpretive authority from literalist textual exegesis to rational theological inquiry; shifts the locus of divine immutability from the text to God's essence, thereby empowering philosophical and rationalist approaches.
% ABSENT_VOICES: Extreme literalists and those who believe in the Qur'an's uncreated nature as a core tenet of faith are often marginalized or suppressed. They would argue this reading diminishes the divine status and authority of the text, leading to dangerous innovations.
% DISAPPEARANCE_RATIONALE: If this theological position vanished, the intellectual landscape of Islamic thought would fundamentally shift. The uncreated view would likely become universally dominant, leading to more rigid interpretations of scripture and a diminished role for rational inquiry in theology and law.
% FOUNDING_PROBLEM: To reconcile the absolute transcendence of God (God is utterly unlike creation) with the temporal nature of revelation (the Qur'an was revealed in time), and to assert the role of human reason in understanding scripture against overly literalist or anthropomorphic interpretations.
% FOUNDING_PROBLEM_CORROBORATION: Rationalist theologians and philosophical schools attest to the ongoing need for this reconciliation in contemporary Islamic thought. Historians of Islamic thought corroborate the historical context and intellectual problems this doctrine sought to address, noting its enduring relevance in debates on reason and revelation.
narrative_ontology:disappearance_verdict(quran_ontological_status__created_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__created_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__created_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(quran_ontological_status__created_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__created_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness (0.6) arises from the shift in interpretive authority and the challenge to established traditionalist power structures. Suppression (0.7) is high due to historical periods of state enforcement (e.g., the Mihna) where this doctrine was imposed, and its ongoing suppression of literalist interpretations. The theater ratio (0.2) is low, as the core debate is genuinely theological, though enforcement mechanisms may have performative aspects. Accessibility collapse (0.3) is low because this reading opens up, rather than closes, interpretive alternatives. Resistance (0.8) was historically very high from traditionalist and literalist factions. The measurement series reflects a period of rising enforcement and extractiveness, followed by a stabilization of its contested status.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of rationalist theologians, this constraint functions as a necessary rope, coordinating theological understanding to preserve divine transcendence and enable intellectual inquiry. For traditionalist jurists and literalist communities, it operates as a snare, extracting their authority and undermining their foundational beliefs about the uncreated nature of the Qur'an. The engine will compute this divergence from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   Rationalist theologians, philosophical schools, and reform movements are beneficiaries (low d) as this reading validates their methods and empowers their interpretive authority. Traditionalist jurists and literalist communities are victims/targets (high d) as their authority and identity are challenged. Political authorities can act as agenda-setters, enforcing this doctrine when it aligns with their interests, making them beneficiaries in such contexts. Analytical observers maintain an objective stance.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    historical_enforcement_ambiguity,
    'Was the historical state enforcement (Mihna) of the created Qur''an doctrine a necessary defense of theological truth and rational inquiry, or an abuse of political power to control religious discourse?',
    'Analysis of primary historical sources, including state decrees, theological treatises, and accounts of those persecuted, to discern stated motivations versus actual effects and power dynamics.',
    'If primarily an abuse of power, the ''state_enforced_creation_reading'' would be more clearly classified as a Snare. If a necessary defense, it would reinforce the ''created_reading''s'' coordination function, even with coercive elements.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_enforcement_ambiguity, empirical, 'Ambiguity regarding the motivations and justification for historical state enforcement of this doctrine.').

omega_variable(
    theological_vs_political_motivation,
    'To what extent was the primary motivation for asserting the createdness of the Qur''an theological purity (preserving divine transcendence) versus political control over religious discourse and scholars?',
    'Detailed historical and intellectual biography of key proponents, examining their writings, political affiliations, and the broader socio-political context of their arguments.',
    'If primarily political, the ''created_reading''s'' coordination function would be seen as a cover for extraction, pushing its classification closer to a Tangled Rope or Snare. If primarily theological, its Rope classification would be more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_political_motivation, conceptual, 'Distinguishing between theological and political drivers behind the doctrine''s promotion.').

omega_variable(
    impact_on_textual_authority,
    'Does classifying the Qur''an as created ultimately diminish its authority as divine guidance and open it to arbitrary human interpretation, or does it enhance its authority by making it more accessible to reason and preventing anthropomorphism?',
    'Longitudinal study of hermeneutical developments in schools of thought that adopted this doctrine versus those that rejected it, assessing the perceived stability and authority of the text over time.',
    'If it leads to perceived diminution of authority, the ''created_reading''s'' long-term viability as a stable coordination mechanism is undermined. If it enhances authority through rational engagement, its Rope classification is strengthened.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(impact_on_textual_authority, conceptual, 'The long-term effect of the ''created'' doctrine on the perceived authority and interpretability of the Qur''an.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__created_reading, 0, 30).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__created_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(qura_tr_t6, quran_ontological_status__created_reading, theater_ratio, 6, 0.15).
narrative_ontology:measurement(qura_tr_t12, quran_ontological_status__created_reading, theater_ratio, 12, 0.2).
narrative_ontology:measurement(qura_tr_t18, quran_ontological_status__created_reading, theater_ratio, 18, 0.18).
narrative_ontology:measurement(qura_tr_t24, quran_ontological_status__created_reading, theater_ratio, 24, 0.19).
narrative_ontology:measurement(qura_tr_t30, quran_ontological_status__created_reading, theater_ratio, 30, 0.2).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__created_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(qura_be_t6, quran_ontological_status__created_reading, base_extractiveness, 6, 0.55).
narrative_ontology:measurement(qura_be_t12, quran_ontological_status__created_reading, base_extractiveness, 12, 0.6).
narrative_ontology:measurement(qura_be_t18, quran_ontological_status__created_reading, base_extractiveness, 18, 0.58).
narrative_ontology:measurement(qura_be_t24, quran_ontological_status__created_reading, base_extractiveness, 24, 0.59).
narrative_ontology:measurement(qura_be_t30, quran_ontological_status__created_reading, base_extractiveness, 30, 0.6).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__created_reading, suppression_requirement, 0, 0.4).
narrative_ontology:measurement(qura_su_t6, quran_ontological_status__created_reading, suppression_requirement, 6, 0.7).
narrative_ontology:measurement(qura_su_t12, quran_ontological_status__created_reading, suppression_requirement, 12, 0.8).
narrative_ontology:measurement(qura_su_t18, quran_ontological_status__created_reading, suppression_requirement, 18, 0.6).
narrative_ontology:measurement(qura_su_t24, quran_ontological_status__created_reading, suppression_requirement, 24, 0.65).
narrative_ontology:measurement(qura_su_t30, quran_ontological_status__created_reading, suppression_requirement, 30, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__created_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__state_enforced_creation_reading).
narrative_ontology:affects_constraint(quran_ontological_status__created_reading, quran_ontological_status__uncreated_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'quran_ontological_status' kernel, focusing on the theological implications of the Qur'an as created speech. It stands in direct theological opposition to the 'uncreated_reading' and provides the intellectual basis for the 'state_enforced_creation_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
