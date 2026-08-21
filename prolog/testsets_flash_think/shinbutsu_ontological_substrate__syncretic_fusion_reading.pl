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
 *   constraint_id: shinbutsu_ontological_substrate__syncretic_fusion_reading
 *   human_readable: Kami-Buddha Ontological Unity (Syncretic Fusion Reading)
 *   domain: religious_studies/japanese_history/commitment_systems
 *
 * SUMMARY:
 *   This constraint story instantiates the 'syncretic_fusion_reading' of the
 *   'shinbutsu_ontological_substrate' kernel. From this perspective, the
 *   unity of kami and buddhas is a fundamental metaphysical truth, not merely
 *   a functional or institutional arrangement. Honji suijaku (original ground
 *   and its trace manifestation) is understood as describing this deep
 *   ontological connection. The constraint is claimed as a Mountain due to
 *   its perceived naturalness and deep cultural embedding, with low
 *   extraction and suppression reflecting its internalized acceptance. The
 *   presence of 'religious_institutions' as beneficiaries triggers False
 *   Summit Mountain detection, prompting an omega variable to explore the
 *   ambiguity between natural law and constructed benefit.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.15).
domain_priors:suppression_score(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.1).
domain_priors:theater_ratio(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, extractiveness, 0.15).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(shinbutsu_ontological_substrate__syncretic_fusion_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_ontological_substrate__syncretic_fusion_reading, mountain).
narrative_ontology:human_readable(shinbutsu_ontological_substrate__syncretic_fusion_reading, "Kami-Buddha Ontological Unity (Syncretic Fusion Reading)").
narrative_ontology:topic_domain(shinbutsu_ontological_substrate__syncretic_fusion_reading, "religious_studies/japanese_history/commitment_systems").

domain_priors:emerges_naturally(shinbutsu_ontological_substrate__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_ontological_substrate__syncretic_fusion_reading, '3b20eddb-c059-4417-8f00-787e796ae3a4').
narrative_ontology:cs_kernel_codification('3b20eddb-c059-4417-8f00-787e796ae3a4', formalized).
narrative_ontology:cs_authority_grounding('3b20eddb-c059-4417-8f00-787e796ae3a4', lineage).
narrative_ontology:cs_interpretation_layer_present('3b20eddb-c059-4417-8f00-787e796ae3a4').
narrative_ontology:cs_reading_relation('3b20eddb-c059-4417-8f00-787e796ae3a4', shinbutsu_ontological_substrate__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('3b20eddb-c059-4417-8f00-787e796ae3a4', shinbutsu_ontological_substrate__incoherent_bundle_reading, forecloses).
narrative_ontology:cs_axiom('3b20eddb-c059-4417-8f00-787e796ae3a4', foundational, kami_buddha_ontological_unity).
narrative_ontology:cs_axiom_status(kami_buddha_ontological_unity, holdable).
narrative_ontology:cs_axiom_grounding('3b20eddb-c059-4417-8f00-787e796ae3a4', kami_buddha_ontological_unity, theological).
narrative_ontology:cs_axiom('3b20eddb-c059-4417-8f00-787e796ae3a4', foundational, honji_suijaku_metaphysical_truth).
narrative_ontology:cs_axiom_status(honji_suijaku_metaphysical_truth, holdable).
narrative_ontology:cs_axiom_grounding('3b20eddb-c059-4417-8f00-787e796ae3a4', honji_suijaku_metaphysical_truth, theological).
narrative_ontology:cs_reference_frame('3b20eddb-c059-4417-8f00-787e796ae3a4', ancient_syncretic_tradition).
narrative_ontology:cs_drift_state('3b20eddb-c059-4417-8f00-787e796ae3a4', contemporary_religious_discourse, gap(stable, minor, true)).
narrative_ontology:cs_created_at('3b20eddb-c059-4417-8f00-787e796ae3a4', '').
narrative_ontology:cs_kernel_id(shinbutsu_ontological_substrate__syncretic_fusion_reading, shinbutsu_ontological_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, religious_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_ontological_substrate__syncretic_fusion_reading, practitioners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% These institutions (temples, shrines, scholarly lineages) propagate and maintain the understanding of kami and buddhas as ontologically unified, deriving legitimacy and continuity from this deep syncretic truth. Their identity is fused with this understanding.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, religious_institutions, agenda_setter,
    institutional, generational, identity_locked, national).

% Individuals who participate in rituals and spiritual practices that seamlessly integrate elements of both Shinto and Buddhism. They experience a coherent spiritual worldview and derive meaning from the unified understanding, which is deeply integrated into their cultural and personal identity.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, practitioners, beneficiary,
    moderate, biographical, identity_locked, local).

% Academics and theologians who study the historical development and philosophical implications of shinbutsu shūgō, often affirming the deep ontological connections described by honji suijaku from a scholarly perspective.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, scholars_of_syncretism, observer,
    analytical, generational, analytical, global).

% Historically, state policies (e.g., Shinbutsu Bunri in the Meiji era) attempted to forcibly separate Shinto and Buddhism. From the perspective of this reading, such attempts were superficial institutional arrangements that failed to alter the underlying metaphysical truth, and their efforts are seen as external impositions rather than reflections of reality.
narrative_ontology:constraint_stakeholder(shinbutsu_ontological_substrate__syncretic_fusion_reading, state_authorities, excluded,
    institutional, generational, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified spiritual and cosmological framework for Japanese society, integrating indigenous beliefs with imported Buddhism into a cohesive system of meaning and practice.
% TRANSFER_FUNCTION: Transfers spiritual authority and legitimacy between kami and buddhas, allowing for a seamless integration of diverse religious practices and consolidating the cultural and religious identity of the populace.
% ABSENT_VOICES: Shinto purists and some Buddhist reformers who advocate for strict separation of kami and buddhas, viewing their coexistence as either a functional arrangement or a historical aberration, not a metaphysical truth. They are excluded from the discourse that affirms this ontological unity.
% DISAPPEARANCE_RATIONALE: If the ontological unity of kami and buddhas were to vanish overnight, the foundational spiritual and cultural fabric of Japan would unravel. Countless rituals, festivals, architectural forms, and personal spiritual practices are predicated on this fusion; their sudden incoherence would necessitate a profound reorganization of religious life and cultural identity.
% FOUNDING_PROBLEM: The challenge of integrating the indigenous kami cults with the newly introduced Buddhist doctrines and practices, creating a coherent and legitimate spiritual system that could encompass both.
% FOUNDING_PROBLEM_CORROBORATION: The continuous practice of syncretic rituals, the enduring presence of jingu-ji (shrine-temple complexes), and the theological writings of prominent figures throughout Japanese history (e.g., Kūkai, Honen) attest to the persistent need for and affirmation of this spiritual unity, corroborated by historical and religious studies from outside the immediate benefiting institutions.
narrative_ontology:disappearance_verdict(shinbutsu_ontological_substrate__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_ontological_substrate__syncretic_fusion_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(shinbutsu_ontological_substrate__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0.15, 'gemini-2.5-flash', 'none', direct).

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
 *   The low extractiveness, suppression, and theater ratio reflect the reading's assertion that this unity is a fundamental truth, deeply internalized and requiring minimal active enforcement or performative maintenance. High accessibility collapse and low resistance further support the Mountain claim, as alternatives to this unified worldview are seen as conceptually difficult or culturally alien. The long interval (1200 years) reflects the historical depth of this syncretic understanding.
 *
 * PERSPECTIVAL GAP:
 *   The core perspectival gap lies between this reading, which asserts deep ontological unity, and sibling readings that view shinbutsu shūgō as a functional arrangement or an incoherent historical accumulation. This constraint's metrics reflect the 'truth' perspective, while other readings would likely assign higher extraction and suppression due to perceived institutional imposition or historical contingency.
 *
 * DIRECTIONALITY LOGIC:
 *   From this reading's perspective, 'religious_institutions' and 'practitioners' are beneficiaries, as they derive spiritual coherence, legitimacy, and cultural identity from this ontological unity. 'Scholars_of_syncretism' are observers, analyzing the truth. 'State_authorities' are 'excluded' because their historical attempts at separation are seen as external impositions that failed to alter the underlying metaphysical reality, thus they are outside the framework of this constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_vs_functional_ambiguity,
    'Is the relationship between kami and buddhas a deep ontological unity (as this reading claims) or primarily a functional arrangement for coexistence (as the domain_partition_reading claims)?',
    'Analysis of pre-Buddhist indigenous beliefs and early Buddhist reception in Japan, focusing on whether initial integration was driven by pragmatic needs or perceived spiritual resonance, and how theological justifications evolved.',
    'If primarily functional, the constraint''s ''emerges_naturally'' claim would be weakened, and its extractiveness and suppression might be re-evaluated as higher, reflecting institutional coordination rather than inherent truth. This would shift its classification away from Mountain.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_vs_functional_ambiguity, conceptual, 'Ambiguity regarding the fundamental nature of kami-buddha interaction.').

omega_variable(
    coherent_kernel_vs_drift_ambiguity,
    'Does a coherent, unified kernel of shinbutsu shūgō exist (as this reading claims), or is it an ''incoherent bundle'' of accumulated institutional drift and state enforcement (as the incoherent_bundle_reading claims)?',
    'Comparative textual analysis of theological treatises across different periods and schools, assessing the consistency of underlying principles versus ad-hoc justifications for institutional practices. Examination of state policies (e.g., Shinbutsu Bunri) and their impact on popular belief versus institutional structure.',
    'If incoherent, the constraint''s ''emerges_naturally'' claim would be invalidated, and its extractiveness and suppression would be seen as much higher, reflecting a Snare or Tangled Rope maintained by coercion and inertia. This would fundamentally alter its classification.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(coherent_kernel_vs_drift_ambiguity, empirical, 'Ambiguity regarding the coherence and intentionality of the syncretic kernel.').

omega_variable(
    natural_law_vs_constructed_beneficiary,
    'Is the ontological unity of kami and buddhas a genuine natural law or metaphysical truth, or is it a constructed constraint that benefits identifiable religious institutions by providing a stable, unified framework for their authority and practices?',
    'Historical and sociological analysis of how the concept of unity was promoted and institutionalized, and whether alternative interpretations were suppressed. Examination of the material and social benefits accruing to religious institutions from this unified framework.',
    'If primarily a constructed constraint benefiting institutions, the ''mountain'' classification would be a ''false summit'', reclassifying it as a Tangled Rope or Snare, with significantly higher effective extraction for practitioners.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(natural_law_vs_constructed_beneficiary, conceptual, 'False Summit Mountain ambiguity: natural law vs. constructed benefit for religious institutions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_ontological_substrate__syncretic_fusion_reading, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 0, 0.04).
narrative_ontology:measurement(shin_tr_t300, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 300, 0.04).
narrative_ontology:measurement(shin_tr_t600, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 600, 0.05).
narrative_ontology:measurement(shin_tr_t900, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 900, 0.05).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_ontological_substrate__syncretic_fusion_reading, theater_ratio, 1200, 0.05).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 0, 0.12).
narrative_ontology:measurement(shin_be_t300, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 300, 0.13).
narrative_ontology:measurement(shin_be_t600, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 600, 0.14).
narrative_ontology:measurement(shin_be_t900, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 900, 0.15).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_ontological_substrate__syncretic_fusion_reading, base_extractiveness, 1200, 0.15).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 0, 0.08).
narrative_ontology:measurement(shin_su_t300, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 300, 0.09).
narrative_ontology:measurement(shin_su_t600, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 600, 0.09).
narrative_ontology:measurement(shin_su_t900, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 900, 0.1).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_ontological_substrate__syncretic_fusion_reading, suppression_requirement, 1200, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_ontological_substrate__syncretic_fusion_reading, identity_coordination).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
