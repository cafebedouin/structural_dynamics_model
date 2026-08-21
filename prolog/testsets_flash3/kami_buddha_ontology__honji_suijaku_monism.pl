% ============================================================================
% CONSTRAINT STORY: kami_buddha_ontology__honji_suijaku_monism
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_kami_buddha_ontology__honji_suijaku_monism, []).

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
 *   constraint_id: kami_buddha_ontology__honji_suijaku_monism
 *   human_readable: Honji Suijaku Monism: Kami as Traces of Buddhas
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   This constraint describes the 'honji suijaku' (original ground and trace
 *   manifestation) theory, a dominant theological framework in pre-modern
 *   Japan that posited indigenous kami as phenomenal traces (suijaku) of
 *   universal Buddhist deities (honji). This reading establishes a
 *   hierarchical ontological identity, with Buddhist entities as the prior,
 *   original ground. It is one reading of the broader 'kami_buddha_ontology'
 *   kernel, which addresses the relationship between Shinto and Buddhism.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(kami_buddha_ontology__honji_suijaku_monism, 0.25).
domain_priors:suppression_score(kami_buddha_ontology__honji_suijaku_monism, 0.35).
domain_priors:theater_ratio(kami_buddha_ontology__honji_suijaku_monism, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, extractiveness, 0.25).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0.35).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(kami_buddha_ontology__honji_suijaku_monism, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(kami_buddha_ontology__honji_suijaku_monism, rope).
narrative_ontology:human_readable(kami_buddha_ontology__honji_suijaku_monism, "Honji Suijaku Monism: Kami as Traces of Buddhas").
narrative_ontology:topic_domain(kami_buddha_ontology__honji_suijaku_monism, "religious_studies/philosophy_of_religion/japanese_cultural_history").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(kami_buddha_ontology__honji_suijaku_monism, '03825f11-26c6-418d-95b5-66dd5c3f2ec2').
narrative_ontology:cs_kernel_codification('03825f11-26c6-418d-95b5-66dd5c3f2ec2', formalized).
narrative_ontology:cs_authority_grounding('03825f11-26c6-418d-95b5-66dd5c3f2ec2', lineage).
narrative_ontology:cs_interpretation_layer_present('03825f11-26c6-418d-95b5-66dd5c3f2ec2').
narrative_ontology:cs_reading_relation('03825f11-26c6-418d-95b5-66dd5c3f2ec2', kami_buddha_ontology__domain_partition, forecloses).
narrative_ontology:cs_reading_relation('03825f11-26c6-418d-95b5-66dd5c3f2ec2', kami_buddha_ontology__incoherent_bundle, influences).
narrative_ontology:cs_axiom('03825f11-26c6-418d-95b5-66dd5c3f2ec2', foundational, buddhas_as_universal_ground).
narrative_ontology:cs_axiom_status(buddhas_as_universal_ground, holdable).
narrative_ontology:cs_axiom_grounding('03825f11-26c6-418d-95b5-66dd5c3f2ec2', buddhas_as_universal_ground, deontological).
narrative_ontology:cs_axiom('03825f11-26c6-418d-95b5-66dd5c3f2ec2', foundational, kami_as_phenomenal_manifestations).
narrative_ontology:cs_axiom_status(kami_as_phenomenal_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('03825f11-26c6-418d-95b5-66dd5c3f2ec2', kami_as_phenomenal_manifestations, conventional).
narrative_ontology:cs_reference_frame('03825f11-26c6-418d-95b5-66dd5c3f2ec2', buddhist_cosmological_supremacy).
narrative_ontology:cs_drift_state('03825f11-26c6-418d-95b5-66dd5c3f2ec2', contemporary_post_shinbutsu_bunri_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('03825f11-26c6-418d-95b5-66dd5c3f2ec2', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, buddhist_scholars).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, syncretic_religious_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, lay_practitioners).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, shinto_priests).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, buddhist_universalism).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, hierarchical_ontology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Developed and propagated the honji suijaku theory, providing a systematic theological framework that integrated indigenous kami into a Buddhist cosmology. They benefit from the intellectual coherence and expanded influence of Buddhism.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, buddhist_scholars, agenda_setter,
    institutional, generational, constrained, national).

% While often adopting syncretic practices, their indigenous traditions are theoretically subordinated to Buddhist metaphysics. They pay by accepting a secondary ontological status for the kami they serve, potentially diluting the unique authority of Shinto.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, shinto_priests, payer,
    moderate, biographical, constrained, local).

% These institutions, often combining elements of both Shinto and Buddhism, benefit from the honji suijaku theory as it provides a coherent theological justification for their existence and practices, attracting adherents from both traditions.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, syncretic_religious_institutions, beneficiary,
    organized, generational, mobile, national).

% Experience a unified religious landscape where they can worship both kami and buddhas without perceived contradiction, simplifying their spiritual practice and worldview. They benefit from the intellectual and ritual coherence.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, lay_practitioners, beneficiary,
    powerless, biographical, mobile, local).

% Reject the honji suijaku theory, advocating for the independent and supreme status of kami. They are excluded from the dominant syncretic discourse and actively resist the Buddhist-centric interpretation, often facing marginalization.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, pure_shinto_revivalists, excluded,
    moderate, generational, identity_locked, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified theological framework for understanding the relationship between indigenous kami and imported Buddhist deities, allowing for coherent worship and institutional integration across diverse religious practices in Japan.
% TRANSFER_FUNCTION: Transfers ontological priority and explanatory power from indigenous Shinto traditions to Buddhist metaphysics, while integrating kami as local manifestations within a universal Buddhist framework.
% ABSENT_VOICES: Pure Shinto revivalists and scholars advocating for the independent and supreme status of kami are marginalized; they would argue that kami are not mere traces but primary, distinct deities, and that the honji suijaku theory is a form of cultural subjugation.
% DISAPPEARANCE_RATIONALE: If honji suijaku monism vanished, the theological justification for centuries of syncretic practice would collapse. Religious institutions would face a fundamental schism, forcing a re-evaluation of the nature of kami and buddhas, leading to a more fragmented religious landscape or a resurgence of distinct Shinto and Buddhist identities.
% FOUNDING_PROBLEM: The problem of integrating indigenous Japanese religious beliefs (kami worship) with the newly introduced, highly systematized Buddhist cosmology, which presented a universalistic claim to truth.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist scholars and syncretic institutions attest that the problem of religious integration and systematization remains relevant. Historians of religion and cultural anthropologists, from outside the directly benefiting parties, corroborate the historical necessity and ongoing function of such a unifying framework in Japanese religious thought.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__honji_suijaku_monism, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__honji_suijaku_monism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__honji_suijaku_monism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(kami_buddha_ontology__honji_suijaku_monism, 'none', 1).
narrative_ontology:epsilon_provenance(kami_buddha_ontology__honji_suijaku_monism, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(kami_buddha_ontology__honji_suijaku_monism_tests).
:- end_tests(kami_buddha_ontology__honji_suijaku_monism_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is relatively low (0.25) because the theory primarily serves a coordination function by integrating two religious systems, rather than overtly extracting resources. However, it does extract ontological priority from Shinto. Suppression (0.35) is moderate, reflecting the intellectual and institutional pressure to conform to this dominant theological framework, though outright coercion was rare. Theater ratio is low (0.1) as the theory was a genuine attempt at theological systematization, not mere performance. Accessibility collapse is high (0.7) because once this framework is adopted, alternative, independent understandings of kami become difficult to maintain within the same intellectual space. Resistance is low (0.15) because the theory was widely accepted for centuries, with significant resistance only emerging much later during periods of Shinto nationalism.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Buddhist scholars, this is a brilliant theological Rope, solving a major integration problem. From the perspective of Shinto priests, it's a Rope with a subtle extractive element, requiring them to accept a secondary status for their deities. The engine's classification will reflect this nuanced divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist scholars and syncretic institutions are beneficiaries, gaining intellectual coherence and broader appeal. Shinto priests are payers, as their traditions are ontologically subordinated. Lay practitioners are beneficiaries, experiencing a unified religious worldview. Pure Shinto revivalists are excluded, as their counter-narrative is outside the dominant discourse.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    ontological_priority_ambiguity,
    'Is the ontological priority of buddhas over kami a necessary theological truth, or a culturally constructed hierarchy reflecting the historical power dynamics of Buddhism in Japan?',
    'Comparative theological analysis across different cultural contexts where Buddhism encountered indigenous traditions, examining whether similar hierarchical integrations were universal or context-specific. Historical analysis of institutional power shifts between Buddhist and Shinto establishments.',
    'If culturally constructed, the ''extractiveness'' of this reading increases, as it reflects a power imposition rather than a neutral theological insight. If necessary, the ''extractiveness'' remains low, as it''s an inherent feature of the coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(ontological_priority_ambiguity, conceptual, 'Ambiguity regarding the source of Buddhist ontological priority.').

omega_variable(
    resistance_underestimation,
    'Does the low ''resistance'' metric accurately reflect the historical reality, or does it underestimate localized, unrecorded forms of resistance from Shinto practitioners who subtly maintained kami independence?',
    'Archaeological and ethnographic studies of local Shinto shrines and practices, examining evidence of resistance to Buddhist integration not captured in official historical records. Analysis of folk religious practices for implicit counter-narratives.',
    'If resistance is underestimated, the ''suppression'' metric for this reading should be higher, indicating that more active (though perhaps diffuse) force was required to maintain the dominant narrative. This would shift the classification towards a more extractive type.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(resistance_underestimation, empirical, 'Potential underestimation of historical resistance to honji suijaku.').

omega_variable(
    sibling_reading_impact,
    'How would the classification of this reading change if the ''domain_partition'' or ''incoherent_bundle'' sibling readings were adopted as the primary framework?',
    'Simulating the constraint''s metrics and stakeholder positions under the alternative ontological premises of the sibling readings.',
    'If ''domain_partition'' were adopted, this reading''s claim of ontological identity would be foreclosed, and its coordination function would be seen as an unnecessary imposition. If ''incoherent_bundle'' were adopted, this reading''s systematizing function would be seen as a theatrical cover for underlying contradictions, increasing its theater_ratio.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sibling_reading_impact, conceptual, 'Impact of adopting sibling readings on this constraint''s classification.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__honji_suijaku_monism, 0, 1200).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0, 0.05).
narrative_ontology:measurement(kami_tr_t300, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 300, 0.08).
narrative_ontology:measurement(kami_tr_t600, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 600, 0.1).
narrative_ontology:measurement(kami_tr_t900, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 900, 0.09).
narrative_ontology:measurement(kami_tr_t1200, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 1200, 0.1).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(kami_be_t300, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 300, 0.25).
narrative_ontology:measurement(kami_be_t600, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 600, 0.3).
narrative_ontology:measurement(kami_be_t900, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 900, 0.28).
narrative_ontology:measurement(kami_be_t1200, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 1200, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(kami_su_t300, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 300, 0.35).
narrative_ontology:measurement(kami_su_t600, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 600, 0.4).
narrative_ontology:measurement(kami_su_t900, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 900, 0.38).
narrative_ontology:measurement(kami_su_t1200, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 1200, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__honji_suijaku_monism, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__domain_partition).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__incoherent_bundle).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'kami_buddha_ontology' kernel. This 'honji_suijaku_monism' reading posits kami as traces of buddhas, establishing a hierarchical ontological identity. It is linked to sibling readings 'domain_partition' and 'incoherent_bundle' which offer alternative interpretations of the kami-buddha relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
