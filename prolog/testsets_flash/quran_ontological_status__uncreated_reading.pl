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
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quran_ontological_status__uncreated_reading
 *   human_readable: Qur'an as Uncreated Eternal Divine Speech (Uncreated Reading)
 *   domain: islamic_theology/philosophy_of_language/political_authority
 *
 * SUMMARY:
 *   This constraint represents the theological position that the Qur'an is
 *   the uncreated, eternal speech of God (kalām Allāh qadīm), coeternal with
 *   God's essence. This reading posits revelation as an ontic constraint, a
 *   permanent feature of reality rather than a created artifact. It maximizes
 *   prophetic authority, privileges literalist hermeneutics, and treats
 *   textual meaning as fixed divine fact. This is one reading of the
 *   'quran_ontological_status' kernel, distinct from 'created_reading' and
 *   'state_enforced_creation_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, 0.1).
domain_priors:suppression_score(quran_ontological_status__uncreated_reading, 0.2).
domain_priors:theater_ratio(quran_ontological_status__uncreated_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, extractiveness, 0.1).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, suppression_requirement, 0.2).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__uncreated_reading, mountain).
narrative_ontology:human_readable(quran_ontological_status__uncreated_reading, "Qur'an as Uncreated Eternal Divine Speech (Uncreated Reading)").
narrative_ontology:topic_domain(quran_ontological_status__uncreated_reading, "islamic_theology/philosophy_of_language/political_authority").

domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__uncreated_reading, '743f5850-475e-4fd8-96a6-a665c74c53eb').
narrative_ontology:cs_kernel_codification('743f5850-475e-4fd8-96a6-a665c74c53eb', fixed_text).
narrative_ontology:cs_authority_grounding('743f5850-475e-4fd8-96a6-a665c74c53eb', lineage).
narrative_ontology:cs_interpretation_layer_present('743f5850-475e-4fd8-96a6-a665c74c53eb').
narrative_ontology:cs_reading_relation('743f5850-475e-4fd8-96a6-a665c74c53eb', quran_ontological_status__created_reading, coexists_with).
narrative_ontology:cs_reading_relation('743f5850-475e-4fd8-96a6-a665c74c53eb', quran_ontological_status__state_enforced_creation_reading, forecloses).
narrative_ontology:cs_axiom('743f5850-475e-4fd8-96a6-a665c74c53eb', foundational, divine_speech_coeternal_with_god).
narrative_ontology:cs_axiom_status(divine_speech_coeternal_with_god, holdable).
narrative_ontology:cs_axiom_grounding('743f5850-475e-4fd8-96a6-a665c74c53eb', divine_speech_coeternal_with_god, deontological).
narrative_ontology:cs_axiom('743f5850-475e-4fd8-96a6-a665c74c53eb', secondary, textual_meaning_fixed_divine_fact).
narrative_ontology:cs_axiom_status(textual_meaning_fixed_divine_fact, holdable).
narrative_ontology:cs_axiom_grounding('743f5850-475e-4fd8-96a6-a665c74c53eb', textual_meaning_fixed_divine_fact, theological).
narrative_ontology:cs_reference_frame('743f5850-475e-4fd8-96a6-a665c74c53eb', ashari_theological_orthodoxy).
narrative_ontology:cs_drift_state('743f5850-475e-4fd8-96a6-a665c74c53eb', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('743f5850-475e-4fd8-96a6-a665c74c53eb', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__uncreated_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, literalist_communities).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, anti_rationalist_schools).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, rational_theologians).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, metaphorical_interpreters).
narrative_ontology:constraint_victim(quran_ontological_status__uncreated_reading, reform_movements).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their authority is grounded in the fixed, eternal nature of the Qur'an, allowing for stable legal interpretation and resistance to theological innovation. This reading maximizes their interpretive power and minimizes challenges to established jurisprudence.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, traditional_jurists, beneficiary,
    institutional, generational, identity_locked, global).

% Find certainty and clarity in the uncreated nature of the Qur'an, which supports a direct, literal understanding of its text and minimizes the need for complex philosophical or allegorical interpretation. Their worldview is affirmed by this ontological status.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, literalist_communities, beneficiary,
    organized, generational, identity_locked, global).

% Benefit from the uncreated reading as it limits the scope for rational inquiry and philosophical speculation into divine attributes, reinforcing a theological stance that prioritizes revelation over reason in matters of faith.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, anti_rationalist_schools, beneficiary,
    organized, generational, identity_locked, global).

% Their efforts to reconcile revelation with philosophical reason are constrained by the uncreated reading, which posits a divine speech that is beyond human categories of creation and temporality, making rationalist interpretations difficult to sustain within the dominant framework.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, rational_theologians, payer,
    moderate, biographical, constrained, global).

% Their hermeneutical approaches, which seek deeper, non-literal meanings, are often marginalized or deemed heterodox by the uncreated reading, which emphasizes the direct, fixed meaning of the divine text.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, metaphorical_interpreters, payer,
    moderate, biographical, constrained, global).

% Seeking textual flexibility for modern challenges, they find their efforts hampered by the uncreated reading, which treats the Qur'an's meaning as immutable and resistant to reinterpretation, thus limiting the scope for progressive reforms.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, reform_movements, payer,
    organized, generational, constrained, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a fixed, immutable source of divine law and theological truth, coordinating belief and practice around a singular, unchallengeable textual authority. This provides a stable foundation for Islamic jurisprudence and dogma.
% TRANSFER_FUNCTION: Transfers interpretive authority and theological legitimacy to those who uphold the uncreated nature of the Qur'an, from those who seek to apply rational or contextual interpretations. It also transfers certainty and stability to adherents.
% ABSENT_VOICES: Philosophers who would argue for a more nuanced understanding of divine attributes and language, and secular scholars who would analyze the Qur'an as a historical text, are largely excluded from the theological discourse that defines its ontological status.
% DISAPPEARANCE_RATIONALE: If the belief in the Qur'an's uncreated nature vanished, the entire edifice of traditional Islamic theology, jurisprudence, and political authority would undergo a profound reordering. Interpretive methods would shift dramatically, and the basis for religious and legal legitimacy would be fundamentally challenged, leading to widespread theological and social upheaval.
% FOUNDING_PROBLEM: Theological disputes in early Islam regarding the nature of God's attributes and the relationship between divine essence and revelation, particularly concerning the Qur'an's status as either coeternal with God or created in time.
% FOUNDING_PROBLEM_CORROBORATION: Theological schools and historical texts from across Islamic history attest to the ongoing nature of this debate, with various factions continuing to defend their positions. The persistence of traditionalist institutions and their curricula further corroborates the live status of this foundational theological problem.
narrative_ontology:disappearance_verdict(quran_ontological_status__uncreated_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__uncreated_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__uncreated_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quran_ontological_status__uncreated_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(quran_ontological_status__uncreated_reading_tests).

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
 *   The constraint is claimed as a Mountain because its proponents assert it as an immutable, divine truth, not a human construct. Extractiveness is low (0.1) because it's primarily a theological claim about the nature of reality, not a direct mechanism for material extraction, though it confers significant authority. Suppression is low (0.2) as its persistence relies more on theological conviction and tradition than active coercion, though historical periods saw suppression of opposing views. Theater ratio is very low (0.05) as the belief is deeply held and foundational, not performative. Accessibility collapse is high (0.9) because, within this framework, there is no 'alternative' to divine truth; resistance is low (0.05) because, while contested historically, it is largely settled within dominant traditionalist schools.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of beneficiaries, this is a foundational truth that provides stability and clarity. From the perspective of payers, it is a rigid dogma that stifles intellectual inquiry and adaptation. The engine's per-seat classification will reflect this divergence, with beneficiaries experiencing it as a Mountain (or Rope of truth) and payers experiencing it as a Snare (or Tangled Rope of dogma).
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional jurists, literalist communities, and anti-rationalist schools are beneficiaries (d near 0.0) as this reading solidifies their interpretive authority, worldview, and theological positions. Rational theologians, metaphorical interpreters, and reform movements are payers (d near 1.0) as their approaches are constrained or marginalized by the fixed, uncreated nature of the text, limiting their flexibility and influence.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_dogma,
    'Is the uncreated nature of the Qur''an a genuine natural law (ontic truth) or a constructed theological dogma that benefits identifiable agents?',
    'Resolution would require a universally accepted epistemological framework for divine attributes, or a historical analysis demonstrating the political contingency of its adoption over competing views.',
    'If a constructed dogma, its classification would shift from Mountain to a more extractive type (e.g., Tangled Rope or Snare), reflecting the beneficiaries'' role in maintaining it for their own authority.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_dogma, conceptual, 'Ambiguity between ontic truth and theological construct.').

omega_variable(
    theological_vs_political_grounding,
    'To what extent is the persistence of the ''uncreated'' reading due to its theological coherence versus its utility in maintaining specific forms of religious and political authority?',
    'Comparative historical analysis of periods where the ''created'' reading gained prominence (e.g., the Mihna) and the political conditions that enabled or suppressed it, versus periods of theological consensus.',
    'If primarily politically grounded, the constraint''s suppression and extractiveness metrics would be re-evaluated upwards, and its classification might shift towards Snare or Tangled Rope, particularly for the ''state_enforced_creation_reading'' sibling.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_vs_political_grounding, empirical, 'Theological vs. political drivers of persistence.').


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
narrative_ontology:measurement(qura_tr_t1500, quran_ontological_status__uncreated_reading, theater_ratio, 1500, 0.05).
narrative_ontology:measurement(qura_tr_t1800, quran_ontological_status__uncreated_reading, theater_ratio, 1800, 0.05).
narrative_ontology:measurement(qura_tr_t2024, quran_ontological_status__uncreated_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(qura_be_t750, quran_ontological_status__uncreated_reading, base_extractiveness, 750, 0.1).
narrative_ontology:measurement(qura_be_t900, quran_ontological_status__uncreated_reading, base_extractiveness, 900, 0.1).
narrative_ontology:measurement(qura_be_t1200, quran_ontological_status__uncreated_reading, base_extractiveness, 1200, 0.1).
narrative_ontology:measurement(qura_be_t1500, quran_ontological_status__uncreated_reading, base_extractiveness, 1500, 0.1).
narrative_ontology:measurement(qura_be_t1800, quran_ontological_status__uncreated_reading, base_extractiveness, 1800, 0.1).
narrative_ontology:measurement(qura_be_t2024, quran_ontological_status__uncreated_reading, base_extractiveness, 2024, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t750, quran_ontological_status__uncreated_reading, suppression_requirement, 750, 0.2).
narrative_ontology:measurement(qura_su_t900, quran_ontological_status__uncreated_reading, suppression_requirement, 900, 0.2).
narrative_ontology:measurement(qura_su_t1200, quran_ontological_status__uncreated_reading, suppression_requirement, 1200, 0.2).
narrative_ontology:measurement(qura_su_t1500, quran_ontological_status__uncreated_reading, suppression_requirement, 1500, 0.2).
narrative_ontology:measurement(qura_su_t1800, quran_ontological_status__uncreated_reading, suppression_requirement, 1800, 0.2).
narrative_ontology:measurement(qura_su_t2024, quran_ontological_status__uncreated_reading, suppression_requirement, 2024, 0.2).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
