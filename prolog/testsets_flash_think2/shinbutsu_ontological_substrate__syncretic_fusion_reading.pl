% ============================================================================
% CONSTRAINT STORY: shinbutsu_ontological_substrate__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_ontological_substrate__syncretic_fusion_reading, []).

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
 *   constraint_id: shinbutsu_ontological_substrate__syncretic_fusion_reading
 *   human_readable: Kami-Buddha Ontological Unity (Syncretic Fusion Reading)
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   This constraint represents the 'syncretic fusion' reading of the
 *   Kami-Buddha ontological substrate, asserting that Kami and Buddhas are
 *   fundamentally unified at a metaphysical level, with honji suijaku
 *   (original ground and manifest trace) describing this deep truth rather
 *   than a mere institutional arrangement. This reading emphasizes high
 *   institutional entanglement and resistance to separation, viewing
 *   syncretism as a core ontological commitment. The claimed type is
 *   'mountain' due to its assertion as a metaphysical truth, but the metrics
 *   reflect the active maintenance and suppression required to uphold this
 *   truth against alternative interpretations.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.45).
domain_priors:suppression_score(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.7).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, extractiveness, 0.45).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__syncretic_fusion_reading, mountain).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__syncretic_fusion_reading, "Kami-Buddha Ontological Unity (Syncretic Fusion Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__syncretic_fusion_reading, "religious_studies/japanese_history/commitment_systems").

domain_priors:requires_active_enforcement(shinbutsu_ontological_substrate__syncretic_fusion_reading).
domain_priors:emerges_naturally(shinbutsu_ontological_substrate__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__syncretic_fusion_reading, '3871a623-21c8-478c-96cd-9ec53584b57b').
narrative_ontology:cs_kernel_codification('3871a623-21c8-478c-96cd-9ec53584b57b', formalized).
narrative_ontology:cs_authority_grounding('3871a623-21c8-478c-96cd-9ec53584b57b', lineage).
narrative_ontology:cs_interpretation_layer_present('3871a623-21c8-478c-96cd-9ec53584b57b').
narrative_ontology:cs_reading_relation('3871a623-21c8-478c-96cd-9ec53584b57b', shinbutsu_ontological_substrate__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('3871a623-21c8-478c-96cd-9ec53584b57b', shinbutsu_ontological_substrate__incoherent_bundle_reading, forecloses).
narrative_ontology:cs_axiom('3871a623-21c8-478c-96cd-9ec53584b57b', foundational, kami_buddha_non_duality).
narrative_ontology:cs_axiom_status(kami_buddha_non_duality, holdable).
narrative_ontology:cs_axiom_grounding('3871a623-21c8-478c-96cd-9ec53584b57b', kami_buddha_non_duality, theological).
narrative_ontology:cs_axiom('3871a623-21c8-478c-96cd-9ec53584b57b', foundational, honji_suijaku_metaphysical_truth).
narrative_ontology:cs_axiom_status(honji_suijaku_metaphysical_truth, holdable).
narrative_ontology:cs_axiom_grounding('3871a623-21c8-478c-96cd-9ec53584b57b', honji_suijaku_metaphysical_truth, theological).
narrative_ontology:cs_reference_frame('3871a623-21c8-478c-96cd-9ec53584b57b', pre_meiji_syncretic_orthodoxy).
narrative_ontology:cs_drift_state('3871a623-21c8-478c-96cd-9ec53584b57b', post_meiji_separation_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('3871a623-21c8-478c-96cd-9ec53584b57b', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretic_scholars_priests).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, traditional_practitioners).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinto_purists).
narrative_ontology:constraint_victim(shinbutsu_ontological_substrate__syncretic_fusion_reading, buddhist_separatists).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These religious authorities interpret and transmit the doctrine of ontological unity, benefiting from the stability and legitimacy it provides to their institutions and practices. Their professional identity is deeply intertwined with this syncretic framework.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretic_scholars_priests, agenda_setter,
    institutional, generational, identity_locked, national).

% Individuals who find spiritual meaning, community, and cultural continuity in the syncretic practices that flow from the belief in Kami-Buddha unity. Their daily religious life is structured by this understanding, making exit from it a profound identity crisis.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, traditional_practitioners, beneficiary,
    moderate, biographical, identity_locked, local).

% Historically, state powers (prior to the Meiji Restoration) benefited from the social cohesion and administrative control offered by a unified religious framework. They actively supported and enforced syncretic interpretations to maintain order and legitimacy.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, state_authorities_pre_meiji, agenda_setter,
    institutional, generational, arbitrage, national).

% Academics who study the historical, sociological, and philosophical aspects of Kami-Buddha syncretism without necessarily adhering to its tenets. They analyze its structure and impact from an external, critical perspective.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, modern_secular_scholars, observer,
    analytical, biographical, analytical, global).

% Advocates for a distinct, 'pure' Shinto tradition, separate from Buddhist influence. They bear the cost of having their alternative, partitioned view marginalized or suppressed by the dominant syncretic framework, facing institutional and intellectual resistance.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinto_purists, payer,
    organized, biographical, constrained, national).

% Advocates for a distinct Buddhist identity and institutional structure, free from syncretic entanglement with Kami worship. They experience the constraint as a barrier to establishing a 'pure' Buddhist practice and doctrine.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, buddhist_separatists, payer,
    organized, biographical, constrained, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_ontological_substrate__syncretic_fusion_reading, syncretic_scholars_priests).
narrative_ontology:fixing_cost_class(shinbutsu_ontological_substrate__syncretic_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: It provides a unified metaphysical framework that integrates diverse religious practices and beliefs, fostering social cohesion and reducing inter-religious conflict by asserting a common ontological ground.
% TRANSFER_FUNCTION: It transfers legitimacy and institutional stability to religious authorities and practices operating within the syncretic framework, while marginalizing or suppressing alternative, separatist religious movements.
% ABSENT_VOICES: Those who would argue for a strict separation of Kami and Buddhas, or who view the syncretic tradition as an incoherent historical accretion rather than a metaphysical truth, are structurally excluded from the dominant discourse and institutional power structures.
% DISAPPEARANCE_RATIONALE: If the belief in Kami-Buddha ontological unity vanished, the foundational logic of many traditional Japanese religious institutions and practices would collapse. The historical Meiji separation (Shinbutsu-bunri) demonstrated the profound societal and institutional rearrangement that occurs when this unity is challenged, leading to the redefinition of religious identities and the restructuring of sacred spaces.
% FOUNDING_PROBLEM: The problem of integrating indigenous Kami worship with the newly introduced Buddhist traditions, seeking to reconcile distinct spiritual paths and prevent religious fragmentation or conflict within society.
% FOUNDING_PROBLEM_CORROBORATION: Syncretic scholars and traditional practitioners attest that the problem of spiritual integration remains live, as the need for a holistic worldview persists. Modern secular scholars corroborate that the historical tension between distinct and unified religious identities continues to be a significant cultural and academic topic, even if the state-mandated separation altered its institutional expression.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__syncretic_fusion_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.45, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, ExtMetricName, E),
    domain_priors:suppression_score(shinbutsu_ontological_substrate__syncretic_fusion_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(shinbutsu_ontological_substrate__syncretic_fusion_reading),
    narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(shinbutsu_ontological_substrate__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The base extractiveness (0.45) is moderate, reflecting the benefits accrued by institutions and practitioners upholding this view, and the costs borne by those advocating for separation. Suppression (0.70) is high, as the persistence of this 'truth' historically involved active marginalization of purist or separatist movements. Theater ratio (0.10) is low, as the constraint is genuinely believed to be a metaphysical truth, not a performance. Accessibility collapse (0.80) is high because, within this framework, alternatives to ontological unity are difficult to conceive. Resistance (0.40) is moderate, reflecting historical and ongoing challenges from purist movements, even if the dominant view was deeply entrenched.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of syncretic scholars and practitioners, this constraint is a fundamental truth that naturally emerges from spiritual experience and tradition, providing profound meaning and cohesion. From the perspective of purists or separatists, it is a constructed imposition that suppresses genuine distinctions and extracts conformity. The engine's classification will highlight this divergence between the claimed 'mountain' and the operational metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   Syncretic scholars/priests and traditional practitioners are beneficiaries, as their roles and practices are validated and sustained by this ontological unity. Pre-Meiji state authorities were also beneficiaries, leveraging the religious unity for social control. Shinto purists and Buddhist separatists are victims, as their efforts to establish distinct religious identities were suppressed or marginalized by the dominant syncretic framework. Modern secular scholars act as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    metaphysical_truth_vs_social_construct,
    'Is the Kami-Buddha ontological unity a genuine metaphysical truth, or a socially constructed and institutionally maintained framework?',
    'Comparative analysis of religious traditions across cultures for similar syncretic patterns, and historical sociological studies of the institutional mechanisms that promoted and enforced this specific interpretation.',
    'If primarily a social construct, the ''mountain'' claim is a false summit, and the constraint''s effective extraction and suppression are higher than its proponents acknowledge, reclassifying it closer to a Snare or Tangled Rope. If a genuine metaphysical truth, the ''mountain'' classification holds.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(metaphysical_truth_vs_social_construct, conceptual, 'Ambiguity between claimed natural law and constructed social reality.').

omega_variable(
    syncretic_fusion_vs_domain_partition,
    'Does the ''syncretic_fusion_reading'' genuinely foreclose the ''domain_partition_reading'' (Kami and Buddhas govern separate domains) within a single coherent framework, or do they merely coexist as competing interpretations?',
    'Detailed textual analysis of foundational syncretic doctrines and their explicit or implicit rejection of strict domain partitioning, alongside historical evidence of how proponents of each view interacted.',
    'If ''forecloses'' is structurally accurate, the syncretic reading actively suppresses the alternative. If they ''coexist_with'', the suppression is less about logical contradiction and more about institutional dominance, potentially lowering the effective suppression metric for the ''syncretic_fusion_reading'' if the alternative is genuinely viable elsewhere.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(syncretic_fusion_vs_domain_partition, conceptual, 'The logical relationship between the syncretic fusion and domain partition readings.').

omega_variable(
    syncretic_fusion_vs_incoherent_bundle,
    'Is the ''syncretic_fusion_reading'' a coherent kernel of unified commitment, or is it an ''incoherent_bundle_reading'' (accumulated institutional drift without a unified commitment)?',
    'Philosophical analysis of the internal consistency of syncretic doctrines and their ability to provide a coherent framework for religious practice, contrasted with historical evidence of opportunistic institutional mergers.',
    'If the ''incoherent_bundle_reading'' is correct, the ''syncretic_fusion_reading'' lacks a genuine coordination function, and its persistence is purely extractive, reclassifying it as a Snare. If coherent, its coordination function is real, supporting a Tangled Rope or even Rope classification (depending on extraction).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(syncretic_fusion_vs_incoherent_bundle, conceptual, 'The coherence and intentionality of the syncretic kernel.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0, 0.1).
narrative_ontology:measurement(shin_tr_t20, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 20, 0.1).
narrative_ontology:measurement(shin_tr_t40, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 40, 0.1).
narrative_ontology:measurement(shin_tr_t60, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 60, 0.1).
narrative_ontology:measurement(shin_tr_t80, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 80, 0.1).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 100, 0.1).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 0, 0.35).
narrative_ontology:measurement(shin_be_t20, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 20, 0.38).
narrative_ontology:measurement(shin_be_t40, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 40, 0.41).
narrative_ontology:measurement(shin_be_t60, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 60, 0.43).
narrative_ontology:measurement(shin_be_t80, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 80, 0.44).
narrative_ontology:measurement(shin_be_t100, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 100, 0.45).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(shin_su_t20, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 20, 0.63).
narrative_ontology:measurement(shin_su_t40, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 40, 0.66).
narrative_ontology:measurement(shin_su_t60, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 60, 0.68).
narrative_ontology:measurement(shin_su_t80, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 80, 0.69).
narrative_ontology:measurement(shin_su_t100, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 100, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__syncretic_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'shinbutsu_ontological_substrate' kernel, each representing a distinct structural claim about the relationship between Kami and Buddhas in Japanese religious thought and practice. This reading asserts ontological unity as a metaphysical truth.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
