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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: quran_ontological_status__uncreated_reading
 *   human_readable: Qur'an as Uncreated Eternal Divine Speech (Traditional Reading)
 *   domain: islamic_theology/philosophy_of_language/political_authority
 *
 * SUMMARY:
 *   This constraint represents the traditional Islamic theological doctrine
 *   that the Qur'an is the uncreated, eternal speech of God (kalām Allāh
 *   qadīm), coeternal with God's essence. This reading posits revelation as
 *   an ontic constraint, a permanent and immutable feature of reality. It
 *   maximizes prophetic authority and privileges literalist hermeneutics,
 *   treating textual meaning as fixed divine fact. This story is one reading
 *   of the 'quran_ontological_status' kernel, specifically the
 *   'uncreated_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(quran_ontological_status__uncreated_reading, 0.15).
domain_priors:suppression_score(quran_ontological_status__uncreated_reading, 0.25).
domain_priors:theater_ratio(quran_ontological_status__uncreated_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, suppression_requirement, 0.25).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(quran_ontological_status__uncreated_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(quran_ontological_status__uncreated_reading, mountain).
narrative_ontology:human_readable(quran_ontological_status__uncreated_reading, "Qur'an as Uncreated Eternal Divine Speech (Traditional Reading)").
narrative_ontology:topic_domain(quran_ontological_status__uncreated_reading, "islamic_theology/philosophy_of_language/political_authority").

domain_priors:emerges_naturally(quran_ontological_status__uncreated_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(quran_ontological_status__uncreated_reading, '52697116-d689-444d-aea9-cb7a1e3bbc81').
narrative_ontology:cs_kernel_codification('52697116-d689-444d-aea9-cb7a1e3bbc81', fixed_text).
narrative_ontology:cs_authority_grounding('52697116-d689-444d-aea9-cb7a1e3bbc81', lineage).
narrative_ontology:cs_interpretation_layer_present('52697116-d689-444d-aea9-cb7a1e3bbc81').
narrative_ontology:cs_reading_relation('52697116-d689-444d-aea9-cb7a1e3bbc81', quran_ontological_status__created_reading, forecloses).
narrative_ontology:cs_reading_relation('52697116-d689-444d-aea9-cb7a1e3bbc81', quran_ontological_status__state_enforced_creation_reading, forecloses).
narrative_ontology:cs_axiom('52697116-d689-444d-aea9-cb7a1e3bbc81', foundational, quran_coeternal_with_god).
narrative_ontology:cs_axiom_status(quran_coeternal_with_god, holdable).
narrative_ontology:cs_axiom_grounding('52697116-d689-444d-aea9-cb7a1e3bbc81', quran_coeternal_with_god, theological).
narrative_ontology:cs_axiom('52697116-d689-444d-aea9-cb7a1e3bbc81', secondary, textual_meaning_immutable).
narrative_ontology:cs_axiom_status(textual_meaning_immutable, holdable).
narrative_ontology:cs_axiom_grounding('52697116-d689-444d-aea9-cb7a1e3bbc81', textual_meaning_immutable, deontological).
narrative_ontology:cs_reference_frame('52697116-d689-444d-aea9-cb7a1e3bbc81', classical_ashari_theology).
narrative_ontology:cs_drift_state('52697116-d689-444d-aea9-cb7a1e3bbc81', contemporary_islamic_thought, gap(stable, minor, true)).
narrative_ontology:cs_created_at('52697116-d689-444d-aea9-cb7a1e3bbc81', '').
narrative_ontology:cs_kernel_id(quran_ontological_status__uncreated_reading, quran_ontological_status).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, traditional_jurists).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, literalist_communities).
narrative_ontology:constraint_beneficiary(quran_ontological_status__uncreated_reading, anti_rationalist_schools).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, prophetic_infallibility).
narrative_ontology:constraint_vindicates(quran_ontological_status__uncreated_reading, textual_inerrancy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Their authority is grounded in the uncreated nature of the Qur'an, which establishes a fixed, eternal source of law and meaning. This position grants them interpretive stability and resistance to reformist challenges. Their professional identity is fused with this doctrine.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, traditional_jurists, beneficiary,
    institutional, generational, identity_locked, global).

% Benefit from the clarity and immutability of divine speech, which supports a direct, unmediated understanding of religious texts and practices. Their communal identity is often defined by adherence to this theological stance.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, literalist_communities, beneficiary,
    organized, generational, identity_locked, global).

% This doctrine reinforces their theological position against rationalist inquiry into divine attributes, asserting that the Qur'an's nature is beyond human reason. Their institutional legitimacy is tied to defending this traditional view.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, anti_rationalist_schools, beneficiary,
    institutional, generational, identity_locked, global).

% Historically marginalized or persecuted for advocating the createdness of the Qur'an, which they believed allowed for greater theological flexibility and rational interpretation. Their views are often suppressed in traditional discourse.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, rational_theologians, excluded,
    moderate, biographical, constrained, regional).

% Their hermeneutical approach, which emphasizes allegorical or contextual readings, is often challenged by the uncreated doctrine's emphasis on literal meaning. They face pressure to conform to more traditional interpretations.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, metaphorical_interpreters, excluded,
    moderate, biographical, constrained, regional).

% Seek textual flexibility to address modern challenges, which is difficult under a doctrine of uncreated, immutable divine speech. They often face strong resistance from traditional authorities and communities.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, reform_movements, excluded,
    organized, generational, constrained, national).

% Study the historical, theological, and political implications of the uncreated Qur'an doctrine, analyzing its impact on Islamic thought, law, and society without being directly subject to its strictures.
narrative_ontology:constraint_stakeholder(quran_ontological_status__uncreated_reading, analytical_observers, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a singular, immutable, and universally authoritative source of divine guidance, coordinating belief and practice across diverse Muslim communities by providing a fixed reference point for all theological and legal discourse.
% TRANSFER_FUNCTION: Transfers ultimate interpretive authority from human reason or contextual understanding to the eternal, uncreated word of God, thereby concentrating hermeneutical power within traditional scholarly institutions that uphold this doctrine.
% ABSENT_VOICES: Rationalist theologians (like the Mu'tazilites) and philosophical schools that sought to reconcile revelation with reason are largely absent from mainstream discourse, having been historically suppressed or marginalized. Their arguments for a created Qur'an, which would allow for greater interpretive flexibility, are excluded.
% DISAPPEARANCE_RATIONALE: If the doctrine of the uncreated Qur'an vanished, the foundations of traditional Islamic theology, jurisprudence, and political authority would be profoundly shaken. Interpretive methodologies would diversify, the authority of traditional institutions would diminish, and new theological schools emphasizing human reason or contextual readings would likely emerge, leading to a fundamental reordering of religious and social structures.
% FOUNDING_PROBLEM: To establish the absolute, unquestionable authority and divine origin of the Qur'an, ensuring its sanctity and immutability against human alteration or rationalist critique, thereby preserving the integrity of revelation.
% FOUNDING_PROBLEM_CORROBORATION: Traditional Islamic scholars and institutions universally attest that the problem of preserving the Qur'an's divine authority and immutability remains live, citing ongoing challenges from secularism, modernism, and diverse interpretive movements. This corroboration comes from within the benefiting parties, as the doctrine itself is foundational to their authority.
narrative_ontology:disappearance_verdict(quran_ontological_status__uncreated_reading, world_rearranges).
narrative_ontology:founding_problem_status(quran_ontological_status__uncreated_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(quran_ontological_status__uncreated_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(quran_ontological_status__uncreated_reading, 'none', 1).
narrative_ontology:epsilon_provenance(quran_ontological_status__uncreated_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The constraint is claimed as a Mountain due to its assertion of coeternality with God, implying an unchangeable, fixed nature. Extractiveness (0.15) is low because the primary function is theological grounding, not material extraction, though it does concentrate interpretive authority. Suppression (0.25) is present but relatively low, reflecting historical marginalization rather than continuous active enforcement in most periods. Accessibility collapse is high (0.88) because, within this theological framework, alternatives to this ontological status are largely foreclosed. Resistance (0.1) is low, as this doctrine has been the dominant and widely accepted view for centuries, with significant opposition being historically suppressed or minor.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the beneficiaries, this is a foundational truth, a Mountain that provides stability and clarity. From the perspective of the excluded, it is a conceptual Snare that limits intellectual inquiry and interpretive flexibility, enforced by tradition and institutional power. The engine's classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Traditional jurists, literalist communities, and anti-rationalist schools are beneficiaries, as their authority, identity, and theological positions are strengthened by this doctrine. Rational theologians, metaphorical interpreters, and reform movements are structurally excluded or marginalized, as their approaches challenge the fixed nature of the text. No direct 'victims' in the sense of material extraction, but rather those whose interpretive freedom or theological positions are constrained.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_constructed_theology,
    'Is the uncreated nature of the Qur''an a genuine theological ''natural law'' (Mountain), or a constructed theological doctrine that benefits identifiable agents (False Summit)?',
    'Analysis of historical theological debates and the political contexts in which this doctrine gained prominence, examining whether its acceptance was driven by intrinsic theological necessity or by the interests of specific power structures.',
    'If found to be a constructed doctrine, the constraint would reclassify from Mountain to Tangled Rope or Snare, reflecting its role in concentrating interpretive authority and suppressing alternative theological views.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_theology, conceptual, 'Ambiguity between inherent theological truth and a doctrine serving institutional interests.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression structural (institutional marginalization, historical persecution) or internalized (cognitive patterns that persist after direct threats are removed)?',
    'Post-exit suppression trajectory: if suppression of alternative interpretations persists even in contexts where direct institutional enforcement is absent, it suggests a partially internalized component.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — the target carries the suppression with them after exit, making reform more difficult.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for theological dissent.').

omega_variable(
    interpretive_flexibility_vs_textual_fixity,
    'Does the doctrine of the uncreated Qur''an inherently foreclose all forms of metaphorical or contextual interpretation, or does it merely constrain them within specific theological boundaries?',
    'Detailed textual analysis of classical and contemporary exegetical works within the uncreated tradition to identify the actual scope of interpretive flexibility permitted, even if limited.',
    'If some flexibility is found, the ''accessibility_collapse'' metric might be slightly lower, and the ''created_reading'' sibling might be less ''foreclosed'' and more ''coexists_with'' in a broader theological framework, albeit with significant pressure.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(interpretive_flexibility_vs_textual_fixity, conceptual, 'The extent to which the uncreated doctrine allows for non-literal interpretations.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(quran_ontological_status__uncreated_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(qura_tr_t0, quran_ontological_status__uncreated_reading, theater_ratio, 0, 0.03).
narrative_ontology:measurement(qura_tr_t300, quran_ontological_status__uncreated_reading, theater_ratio, 300, 0.04).
narrative_ontology:measurement(qura_tr_t600, quran_ontological_status__uncreated_reading, theater_ratio, 600, 0.04).
narrative_ontology:measurement(qura_tr_t900, quran_ontological_status__uncreated_reading, theater_ratio, 900, 0.05).
narrative_ontology:measurement(qura_tr_t1200, quran_ontological_status__uncreated_reading, theater_ratio, 1200, 0.05).

% Extraction over time
narrative_ontology:measurement(qura_be_t0, quran_ontological_status__uncreated_reading, base_extractiveness, 0, 0.1).
narrative_ontology:measurement(qura_be_t300, quran_ontological_status__uncreated_reading, base_extractiveness, 300, 0.12).
narrative_ontology:measurement(qura_be_t600, quran_ontological_status__uncreated_reading, base_extractiveness, 600, 0.13).
narrative_ontology:measurement(qura_be_t900, quran_ontological_status__uncreated_reading, base_extractiveness, 900, 0.14).
narrative_ontology:measurement(qura_be_t1200, quran_ontological_status__uncreated_reading, base_extractiveness, 1200, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(qura_su_t0, quran_ontological_status__uncreated_reading, suppression_requirement, 0, 0.2).
narrative_ontology:measurement(qura_su_t300, quran_ontological_status__uncreated_reading, suppression_requirement, 300, 0.22).
narrative_ontology:measurement(qura_su_t600, quran_ontological_status__uncreated_reading, suppression_requirement, 600, 0.23).
narrative_ontology:measurement(qura_su_t900, quran_ontological_status__uncreated_reading, suppression_requirement, 900, 0.24).
narrative_ontology:measurement(qura_su_t1200, quran_ontological_status__uncreated_reading, suppression_requirement, 1200, 0.25).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(quran_ontological_status__uncreated_reading, identity_coordination).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, islamic_legal_methodology).
narrative_ontology:affects_constraint(quran_ontological_status__uncreated_reading, prophetic_sunna_authority).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'quran_ontological_status' kernel. This 'uncreated_reading' asserts the Qur'an's coeternality with God, influencing and being influenced by other theological and legal constraints.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
