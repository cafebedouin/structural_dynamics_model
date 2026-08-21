% ============================================================================
% CONSTRAINT STORY: quran_ontological_status__uncreated_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_quran_ontological_status__uncreated_reading, []).

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
    domain_priors:emerges_naturally/1,
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
 *   constraint_id: quran_ontological_status__uncreated_reading
 *   human_readable: Qur'an as Uncreated Eternal Divine Speech
 *   domain: islamic_theology/philosophy_of_language/political_authority
 *
 * SUMMARY:
 *   This constraint instantiates the 'uncreated_reading' of the Qur'an's
 *   ontological status, asserting that the Qur'an is eternal divine speech
 *   (kalām Allāh qadīm) coeternal with God. From this perspective, revelation
 *   itself is an ontic constraint, a permanent feature of reality. This
 *   reading maximizes prophetic authority, privileges literalist
 *   hermeneutics, and treats textual meaning as a fixed divine fact. While
 *   claimed as a Mountain (an ontological truth), the presence of
 *   identifiable beneficiaries and victims means the engine will evaluate it
 *   as a 'false summit' candidate, detecting the structural benefits derived
 *   from this theological claim.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, 0.05).
domain_priors:suppression_score(quran_ontological_status__uncreated_reading, 0.05).
domain_priors:theater_ratio(quran_ontological_status__uncreated_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, extractiveness, 0.05).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, suppression_requirement, 0.05).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__uncreated_reading, mountain).
narrative_ontology:human_readable(quran_ontological_status__uncreated_reading, "Qur'an as Uncreated Eternal Divine Speech").
narrative_ontology:topic_domain(quran_ontological_status__uncreated_reading, "islamic_theology/philosophy_of_language/political_authority").

domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__uncreated_reading, '43d39514-f896-4c9e-afed-111510a4d3d6').
narrative_ontology:cs_kernel_codification('43d39514-f896-4c9e-afed-111510a4d3d6', fixed_text).
narrative_ontology:cs_authority_grounding('43d39514-f896-4c9e-afed-111510a4d3d6', lineage).
narrative_ontology:cs_interpretation_layer_present('43d39514-f896-4c9e-afed-111510a4d3d6').
narrative_ontology:cs_reading_relation('43d39514-f896-4c9e-afed-111510a4d3d6', quran_ontological_status__created_reading, forecloses).
narrative_ontology:cs_reading_relation('43d39514-f896-4c9e-afed-111510a4d3d6', quran_ontological_status__state_enforced_creation_reading, forecloses).
narrative_ontology:cs_axiom('43d39514-f896-4c9e-afed-111510a4d3d6', foundational, quran_coeternal_with_god).
narrative_ontology:cs_axiom_status(quran_coeternal_with_god, holdable).
narrative_ontology:cs_axiom_grounding('43d39514-f896-4c9e-afed-111510a4d3d6', quran_coeternal_with_god, theological).
narrative_ontology:cs_axiom('43d39514-f896-4c9e-afed-111510a4d3d6', secondary, textual_meaning_fixed).
narrative_ontology:cs_axiom_status(textual_meaning_fixed, holdable).
narrative_ontology:cs_axiom_grounding('43d39514-f896-4c9e-afed-111510a4d3d6', textual_meaning_fixed, deontological).
narrative_ontology:cs_reference_frame('43d39514-f896-4c9e-afed-111510a4d3d6', classical_sunni_orthodoxy).
narrative_ontology:cs_drift_state('43d39514-f896-4c9e-afed-111510a4d3d6', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('43d39514-f896-4c9e-afed-111510a4d3d6', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__uncreated_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, literalist_communities).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, anti_rationalist_schools).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, rational_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, metaphorical_interpreters).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, reform_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Uphold the doctrine of the Qur'an's uncreatedness as foundational to Islamic law and theology. They derive immense interpretive authority and institutional legitimacy from this fixed, eternal source, resisting any attempts to re-contextualize or relativize its meaning.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, traditional_jurists, agenda_setter,
    institutional, generational, identity_locked, global).

% Find certainty and clear guidance in the uncreated nature of the Qur'an, which supports a literalist approach to scripture. Their worldview is structured around the immutability of divine text, and they benefit from the stability and perceived authenticity this doctrine provides.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, literalist_communities, beneficiary,
    organized, biographical, identity_locked, global).

% Benefit from the uncreated doctrine as it limits the scope for rationalist inquiry and philosophical speculation into divine attributes. It reinforces a theological stance that prioritizes revelation over reason in matters of faith.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, anti_rationalist_schools, beneficiary,
    organized, biographical, identity_locked, global).

% Historically argued for the Qur'an's createdness to preserve God's absolute transcendence and avoid anthropomorphism. They bear the cost of being marginalized or condemned by traditionalist establishments, facing intellectual and sometimes physical suppression for their views.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, rational_theologians, payer,
    powerful, biographical, constrained, global).

% Seek to interpret the Qur'an allegorically or contextually, which is often seen as undermining the fixed, eternal nature of divine speech. They face accusations of heresy or innovation from literalist and traditionalist factions, limiting their influence and acceptance.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, metaphorical_interpreters, payer,
    moderate, biographical, constrained, global).

% Advocate for re-readings of Islamic texts to address modern challenges, often requiring a more flexible understanding of scripture. The uncreated doctrine acts as a significant barrier to their efforts, as it entrenches literalism and resistance to change, forcing them to operate at the margins or face severe opposition.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, reform_movements, payer,
    organized, generational, constrained, global).

% Analyze the theological and philosophical implications of the doctrine, its historical development, and its impact on Islamic thought and society, without being directly bound by its tenets.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, philosophical_inquirers, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:fixing_cost_class(quran_ontological_status__uncreated_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a fixed, immutable, and universally accessible source of divine truth and guidance, establishing a common theological foundation for diverse Muslim communities.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority and theological certainty from God to specific human interpreters (traditional jurists, literalist schools) who uphold the uncreated doctrine, while marginalizing those who seek alternative readings.
% ABSENT_VOICES: Those who seek a more flexible, contextual, or rational interpretation of scripture, as well as those who prioritize God's absolute transcendence over the co-eternality of His speech, are structurally excluded or suppressed from mainstream theological discourse.
% DISAPPEARANCE_RATIONALE: If the doctrine of the Qur'an's uncreatedness vanished overnight, the entire edifice of traditional Islamic law, theology, and political authority would lose its foundational premise. Interpretive methodologies would shift dramatically, opening space for diverse readings and potentially leading to a profound reorganization of religious institutions and communal identity.
% FOUNDING_PROBLEM: To establish an unchallengeable, eternal, and perfectly preserved source of divine truth and guidance for humanity, ensuring its authority and immutability against human alteration or temporal contingency.
% FOUNDING_PROBLEM_CORROBORATION: The problem of preserving divine truth and ensuring its unchallengeable authority is attested by centuries of traditional Islamic scholarship, religious institutions, and the lived faith of billions of Muslims who continue to uphold this doctrine as fundamental. While rationalist schools historically contested it, the traditionalist view has largely prevailed and is widely affirmed by religious authorities outside the immediate beneficiary groups.
narrative_ontology:disappearance_verdict(quran_ontological_status__uncreated_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__uncreated_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__uncreated_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(quran_ontological_status__uncreated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__uncreated_reading, 0.05, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__uncreated_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, ExtMetricName, E),
    domain_priors:suppression_score(quran_ontological_status__uncreated_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(quran_ontological_status__uncreated_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base metrics reflect the claim's nature as an ontological truth: minimal extractiveness, suppression, and theater, with high accessibility collapse (alternatives are ontologically foreclosed) and low resistance (from the perspective of those who accept it). The 'mountain' claim is based on the theological assertion of its inherent, unchangeable nature. The engine's FSM will detect the beneficiaries and reclassify, which is the intended measurement of a 'false summit' where a natural law claim serves specific interests.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of its beneficiaries, the uncreated Qur'an is a self-evident, immutable truth, a pure Mountain. From the perspective of its victims, it functions as a powerful, identity-locking constraint that suppresses alternative interpretations and limits intellectual freedom within the theological domain. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional jurists, literalist communities, and anti-rationalist schools are structural beneficiaries, gaining interpretive authority, certainty, and institutional legitimacy from the fixed nature of divine speech. Rational theologians, metaphorical interpreters, and reform movements are targets, as their approaches are constrained or delegitimized by this doctrine. Philosophical inquirers serve as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_constructed_truth,
    'Is the Qur''an''s uncreated status a genuine ontological truth, or a constructed theological claim that benefits identifiable agents by fixing interpretive authority?',
    'Philosophical and theological analysis of the arguments for and against co-eternality, coupled with sociological analysis of the power dynamics and institutional benefits derived from each position.',
    'If primarily a constructed claim, the constraint''s effective extractiveness and suppression are higher than the base metrics suggest, reclassifying it from a Mountain to a more extractive type (e.g., Tangled Rope or Snare).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_vs_constructed_truth, conceptual, 'Ambiguity between ontological fact and theological construct.').

omega_variable(
    theological_vs_political_enforcement,
    'To what extent does the ''uncreated'' doctrine enable or constrain political authority, and how does this interact with its theological claims?',
    'Historical analysis of periods where political power actively enforced or suppressed this doctrine (e.g., the Mihna for the ''created'' doctrine), and contemporary analysis of state-sponsored religious institutions.',
    'If the doctrine is found to be a significant tool for political legitimation or control, its effective suppression and extractiveness would be amplified, particularly for those who challenge both its theological and political implications.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_political_enforcement, empirical, 'Interaction between theological doctrine and political power.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__uncreated_reading, 750, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t750, quran_ontological_status__uncreated_reading, theater_ratio, 750, 0.05).
narrative_ontology:measurement(qura_tr_t900, quran_ontological_status__uncreated_reading, theater_ratio, 900, 0.05).
narrative_ontology:measurement(qura_tr_t1200, quran_ontological_status__uncreated_reading, theater_ratio, 1200, 0.05).
narrative_ontology:measurement(qura_tr_t1600, quran_ontological_status__uncreated_reading, theater_ratio, 1600, 0.05).
narrative_ontology:measurement(qura_tr_t2024, quran_ontological_status__uncreated_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(qura_be_t750, quran_ontological_status__uncreated_reading, base_extractiveness, 750, 0.05).
narrative_ontology:measurement(qura_be_t900, quran_ontological_status__uncreated_reading, base_extractiveness, 900, 0.05).
narrative_ontology:measurement(qura_be_t1200, quran_ontological_status__uncreated_reading, base_extractiveness, 1200, 0.05).
narrative_ontology:measurement(qura_be_t1600, quran_ontological_status__uncreated_reading, base_extractiveness, 1600, 0.05).
narrative_ontology:measurement(qura_be_t2024, quran_ontological_status__uncreated_reading, base_extractiveness, 2024, 0.05).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t750, quran_ontological_status__uncreated_reading, suppression_requirement, 750, 0.05).
narrative_ontology:measurement(qura_su_t900, quran_ontological_status__uncreated_reading, suppression_requirement, 900, 0.05).
narrative_ontology:measurement(qura_su_t1200, quran_ontological_status__uncreated_reading, suppression_requirement, 1200, 0.05).
narrative_ontology:measurement(qura_su_t1600, quran_ontological_status__uncreated_reading, suppression_requirement, 1600, 0.05).
narrative_ontology:measurement(qura_su_t2024, quran_ontological_status__uncreated_reading, suppression_requirement, 2024, 0.05).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__uncreated_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, prophetic_authority_infallibility).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, sharia_interpretive_methodology).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__created_reading).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, quran_ontological_status__state_enforced_creation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'quran_ontological_status' kernel. Its 'uncreated' claim directly contradicts the 'created' claims of its sibling readings, leading to fundamental theological and political divergences.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
