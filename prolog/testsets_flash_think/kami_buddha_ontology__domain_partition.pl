% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__domain_partition
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__domain_partition, []).

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
 *   constraint_id: kami_buddha_ontology__domain_partition
 *   human_readable: Kami-Buddha Ontological Domain Partition
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   This constraint describes the conceptual framework within Japanese
 *   religious history where Kami (Shinto deities) and Buddhas are understood
 *   as ontologically distinct entities, each governing separate functional
 *   domains: Shinto for life, purity, and the living; Buddhism for death,
 *   impurity, and the deceased. This reading emphasizes functional
 *   complementarity without theoretical fusion or hierarchy, providing a
 *   stable framework for religious practice. It is one reading of the broader
 *   'kami_buddha_ontology' kernel, which has been subject to various
 *   interpretations over centuries.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__domain_partition, 0.08).
domain_priors:suppression_score(kami_buddha_ontology__domain_partition, 0.12).
domain_priors:theater_ratio(kami_buddha_ontology__domain_partition, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, extractiveness, 0.08).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, suppression_requirement, 0.12).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, accessibility_collapse, 0.88).
narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, resistance, 0.03).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__domain_partition, mountain).
narrative_ontology:human_readable(kami_buddha_ontology__domain_partition, "Kami-Buddha Ontological Domain Partition").
narrative_ontology:topic_domain(kami_buddha_ontology__domain_partition, "religious_studies/philosophy_of_religion/japanese_cultural_history").

domain_priors:emerges_naturally(kami_buddha_ontology__domain_partition).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__domain_partition, '0ec09942-c301-4c7d-89bb-09b003f78e20').
narrative_ontology:cs_kernel_codification('0ec09942-c301-4c7d-89bb-09b003f78e20', formalized).
narrative_ontology:cs_authority_grounding('0ec09942-c301-4c7d-89bb-09b003f78e20', lineage).
narrative_ontology:cs_interpretation_layer_present('0ec09942-c301-4c7d-89bb-09b003f78e20').
narrative_ontology:cs_reading_relation('0ec09942-c301-4c7d-89bb-09b003f78e20', kami_buddha_ontology__honji_suijaku_monism, forecloses).
narrative_ontology:cs_reading_relation('0ec09942-c301-4c7d-89bb-09b003f78e20', kami_buddha_ontology__incoherent_bundle, coexists_with).
narrative_ontology:cs_axiom('0ec09942-c301-4c7d-89bb-09b003f78e20', foundational, kami_buddha_ontological_distinction).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_distinction, holdable).
narrative_ontology:cs_axiom_grounding('0ec09942-c301-4c7d-89bb-09b003f78e20', kami_buddha_ontological_distinction, deontological).
narrative_ontology:cs_axiom('0ec09942-c301-4c7d-89bb-09b003f78e20', secondary, functional_domain_partition).
narrative_ontology:cs_axiom_status(functional_domain_partition, holdable).
narrative_ontology:cs_axiom_grounding('0ec09942-c301-4c7d-89bb-09b003f78e20', functional_domain_partition, conventional).
narrative_ontology:cs_reference_frame('0ec09942-c301-4c7d-89bb-09b003f78e20', early_heian_syncretism).
narrative_ontology:cs_drift_state('0ec09942-c301-4c7d-89bb-09b003f78e20', contemporary_religious_studies, gap(stable, minor, true)).
narrative_ontology:cs_created_at('0ec09942-c301-4c7d-89bb-09b003f78e20', '').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__domain_partition, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, shinto_priests).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, buddhist_monastics).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__domain_partition, japanese_lay_practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Administer Shinto rites related to life, purity, and the living. This conceptual partition provides a clear mandate for their ritual authority and domain of practice, benefiting from the clarity it provides.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, shinto_priests, agenda_setter,
    institutional, generational, identity_locked, national).

% Administer Buddhist rites related to death, impurity, and the deceased. This conceptual partition provides a clear mandate for their ritual authority and domain of practice, benefiting from the clarity it provides.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, buddhist_monastics, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefit from a clear, complementary framework for religious practice, knowing which tradition to approach for specific life events (e.g., Shinto for birth/marriage, Buddhism for funerals). This reduces confusion and provides a stable cultural script.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, japanese_lay_practitioners, beneficiary,
    moderate, biographical, constrained, local).

% Analyze and interpret the historical and philosophical development of this conceptual partition. They do not directly participate in its enforcement or benefit from its operation, but their work contributes to its intellectual maintenance.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__domain_partition, religious_scholars, observer,
    analytical, generational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__domain_partition, diffuse).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__domain_partition, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a clear, complementary division of religious labor between Shinto and Buddhism in Japan, preventing conflict and confusion in ritual practice and theological understanding for practitioners.
% TRANSFER_FUNCTION: Transfers clarity, ritual efficacy, and institutional mandates to Shinto priests and Buddhist monastics within their respective domains, and provides coherent guidance for Japanese lay practitioners.
% ABSENT_VOICES: Those who seek a unified, monistic theological understanding of kami and buddhas (e.g., proponents of Honji Suijaku theories) or those who challenge the functional division as arbitrary or historically contingent. They are present in academic discourse but not in the direct operational framing of this partition.
% DISAPPEARANCE_RATIONALE: If this conceptual partition vanished, Japanese religious practice would become deeply confused, leading to significant institutional overlap, theological conflict, and a loss of clear ritual mandates for both Shinto and Buddhist institutions. Lay practitioners would lose a fundamental cultural script for navigating life and death events.
% FOUNDING_PROBLEM: To integrate two distinct religious traditions (indigenous Shinto and imported Buddhism) that arrived in Japan, without one subsuming the other, and to provide coherent, non-conflicting guidance for practitioners across the spectrum of life events.
% FOUNDING_PROBLEM_CORROBORATION: Historical records of syncretic practices, religious texts articulating the functional division, and anthropological studies of contemporary Japanese religious life consistently corroborate the enduring need for such a framework, even as its specific interpretations evolve. This corroboration comes from outside the immediate benefiting institutions.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__domain_partition, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__domain_partition, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__domain_partition, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(kami_buddha_ontology__domain_partition, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__domain_partition, 0.08, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__domain_partition_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, ExtMetricName, E),
    domain_priors:suppression_score(kami_buddha_ontology__domain_partition, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(kami_buddha_ontology__domain_partition),
    narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(kami_buddha_ontology__domain_partition, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(kami_buddha_ontology__domain_partition_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is claimed as a Mountain due to its assertion of fundamental ontological distinction and functional division, which, within this reading, is treated as an inherent truth about the spiritual landscape. Extractiveness is low (0.08) as its primary function is coordination and clarity, not rent-seeking. Suppression is low (0.12) because it's a conceptual framework, not enforced by coercion, though deviation would lead to theological disagreement. Theater ratio is low (0.05) as the framework is genuinely functional. Accessibility collapse is high (0.88) because, within this reading, alternatives to this fundamental division are conceptually difficult to maintain. Resistance is low (0.03) as this reading is largely accepted by its proponents.
 *
 * PERSPECTIVAL GAP:
 *   Within this 'domain_partition' reading, the framework is seen as a natural and beneficial division. Other readings, such as 'honji_suijaku_monism' (which posits kami as manifestations of buddhas) or 'incoherent_bundle' (which views the syncretism as contradictory), would experience this same historical phenomenon very differently, highlighting the perspectival nature of ontological claims.
 *
 * DIRECTIONALITY LOGIC:
 *   Shinto priests and Buddhist monastics are agenda-setters and beneficiaries, as the framework clarifies their institutional roles and ritual authority. Japanese lay practitioners are beneficiaries, gaining clear guidance for religious life. Religious scholars are observers, analyzing the framework without direct participation or benefit.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    natural_law_vs_cultural_construct,
    'Is this ontological distinction a genuine natural law of the spiritual realm, or a culturally constructed framework that benefits identifiable religious institutions by clarifying their roles?',
    'Comparative religious studies across cultures, historical analysis of the emergence of this specific partition, and theological debate on the nature of divine entities. If the partition is found to be highly contingent on Japanese cultural history, it leans towards a construct.',
    'If a cultural construct, the ''mountain'' claim is weakened, and the constraint might reclassify towards a ''rope'' or ''tangled_rope'' if the benefits to institutions are found to be more concentrated and less diffuse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(natural_law_vs_cultural_construct, conceptual, 'Ambiguity regarding the naturalness of the kami-buddha domain partition.').

omega_variable(
    domain_partition_vs_honji_suijaku,
    'Is the ontological distinction between kami and buddhas truly fundamental, or are kami ultimately manifestations (suijaku) of original buddhas/bodhisattvas (honji), as posited by the honji_suijaku_monism reading?',
    'Deep theological and philosophical analysis of primary texts from both Shinto and Buddhist traditions, and historical examination of the periods when these theories were most prominent. Resolution depends on which interpretive framework is adopted.',
    'If honji_suijaku_monism is adopted, this ''domain_partition'' reading would be foreclosed, and the underlying constraint would shift to one emphasizing ontological unity and hierarchical relationship, likely with different beneficiaries and extraction patterns.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(domain_partition_vs_honji_suijaku, conceptual, 'Contest between ontological distinction and monistic identity of kami and buddhas.').

omega_variable(
    coherence_vs_incoherence,
    'Is the functional domain partition a coherent and stable conceptual framework, or is Shinbutsu-shugo an ''incoherent_bundle'' of contradictory commitments, as argued by some scholars?',
    'Detailed analysis of the internal consistency of the ''domain_partition'' framework in practice and theory, examining whether its principles are consistently applied or if it masks underlying contradictions. Resolution depends on the criteria for ''coherence''.',
    'If deemed an ''incoherent_bundle'', the constraint''s stability and functional clarity would be undermined, potentially reclassifying it as a ''piton'' (maintained by inertia despite internal contradictions) or a ''snare'' (if the incoherence serves to obscure extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coherence_vs_incoherence, conceptual, 'Debate over the internal coherence of the kami-buddha syncretism.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__domain_partition, 800, 2000).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t800, kami_buddha_ontology__domain_partition, theater_ratio, 800, 0.05).
narrative_ontology:measurement(kami_tr_t1100, kami_buddha_ontology__domain_partition, theater_ratio, 1100, 0.05).
narrative_ontology:measurement(kami_tr_t1400, kami_buddha_ontology__domain_partition, theater_ratio, 1400, 0.05).
narrative_ontology:measurement(kami_tr_t1700, kami_buddha_ontology__domain_partition, theater_ratio, 1700, 0.05).
narrative_ontology:measurement(kami_tr_t2000, kami_buddha_ontology__domain_partition, theater_ratio, 2000, 0.05).

% Extraction over time
narrative_ontology:measurement(kami_be_t800, kami_buddha_ontology__domain_partition, base_extractiveness, 800, 0.08).
narrative_ontology:measurement(kami_be_t1100, kami_buddha_ontology__domain_partition, base_extractiveness, 1100, 0.08).
narrative_ontology:measurement(kami_be_t1400, kami_buddha_ontology__domain_partition, base_extractiveness, 1400, 0.08).
narrative_ontology:measurement(kami_be_t1700, kami_buddha_ontology__domain_partition, base_extractiveness, 1700, 0.08).
narrative_ontology:measurement(kami_be_t2000, kami_buddha_ontology__domain_partition, base_extractiveness, 2000, 0.08).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t800, kami_buddha_ontology__domain_partition, suppression_requirement, 800, 0.12).
narrative_ontology:measurement(kami_su_t1100, kami_buddha_ontology__domain_partition, suppression_requirement, 1100, 0.12).
narrative_ontology:measurement(kami_su_t1400, kami_buddha_ontology__domain_partition, suppression_requirement, 1400, 0.12).
narrative_ontology:measurement(kami_su_t1700, kami_buddha_ontology__domain_partition, suppression_requirement, 1700, 0.12).
narrative_ontology:measurement(kami_su_t2000, kami_buddha_ontology__domain_partition, suppression_requirement, 2000, 0.12).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
