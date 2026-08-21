% ============================================================================
% CONSTRAINT STORY: shinbutsu_coexistence_commitment__syncretic_fusion_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_shinbutsu_coexistence_commitment__syncretic_fusion_reading, []).

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
 *   constraint_id: shinbutsu_coexistence_commitment__syncretic_fusion_reading
 *   human_readable: Honji Suijaku Doctrine (Syncretic Fusion Reading)
 *   domain: religious_studies/philosophy_of_religion/japanese_history
 *
 * SUMMARY:
 *   This constraint describes the honji suijaku doctrine, which posited kami
 *   as local manifestations of universal Buddhist truths, from the 'syncretic
 *   fusion' reading. This reading emphasizes the ontological unification of
 *   kami and Buddhas, creating a single coherent religious system in
 *   pre-Meiji Japan. It was a dominant theological framework that shaped
 *   religious institutions and practices for centuries. The constraint is
 *   claimed as a Rope because, from this reading's perspective, it genuinely
 *   solved a coordination problem by integrating diverse religious
 *   traditions, even if it involved some degree of subsumption for local kami
 *   cults. The metrics reflect a moderate level of extraction and suppression
 *   inherent in maintaining this elite-driven theological coherence.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.35).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.45).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "Honji Suijaku Doctrine (Syncretic Fusion Reading)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "religious_studies/philosophy_of_religion/japanese_history").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '3155f697-93d0-4772-975b-5dc4a7d15214').
narrative_ontology:cs_kernel_codification('3155f697-93d0-4772-975b-5dc4a7d15214', formalized).
narrative_ontology:cs_authority_grounding('3155f697-93d0-4772-975b-5dc4a7d15214', lineage).
narrative_ontology:cs_interpretation_layer_present('3155f697-93d0-4772-975b-5dc4a7d15214').
narrative_ontology:cs_reading_relation('3155f697-93d0-4772-975b-5dc4a7d15214', shinbutsu_coexistence_commitment__domain_partition_reading, coexists_with).
narrative_ontology:cs_reading_relation('3155f697-93d0-4772-975b-5dc4a7d15214', shinbutsu_coexistence_commitment__incoherent_bundle_reading, coexists_with).
narrative_ontology:cs_axiom('3155f697-93d0-4772-975b-5dc4a7d15214', foundational, kami_as_buddhist_manifestations).
narrative_ontology:cs_axiom_status(kami_as_buddhist_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('3155f697-93d0-4772-975b-5dc4a7d15214', kami_as_buddhist_manifestations, theological).
narrative_ontology:cs_axiom('3155f697-93d0-4772-975b-5dc4a7d15214', foundational, ontological_unity_of_divine).
narrative_ontology:cs_axiom_status(ontological_unity_of_divine, holdable).
narrative_ontology:cs_axiom_grounding('3155f697-93d0-4772-975b-5dc4a7d15214', ontological_unity_of_divine, deontological).
narrative_ontology:cs_reference_frame('3155f697-93d0-4772-975b-5dc4a7d15214', heian_syncretic_synthesis).
narrative_ontology:cs_drift_state('3155f697-93d0-4772-975b-5dc4a7d15214', edo_period_kokugaku_challenge, gap(repudiation_pressure, substantial, false)).
narrative_ontology:cs_created_at('3155f697-93d0-4772-975b-5dc4a7d15214', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_clergy).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, imperial_court).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_elites).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_kami_cults).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, unaffiliated_practitioners).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_universalism).
narrative_ontology:constraint_vindicates(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kami_as_local_manifestations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary interpreters and enforcers of honji suijaku, benefiting from the integration of kami worship into Buddhist institutional structures and the associated landholdings and patronage. They provide the theological framework and administer the jinguji (shrine-temple complexes).
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_clergy, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the ideological coherence and political stability offered by a unified religious system. The doctrine legitimizes imperial authority by integrating local kami into a universal framework, reinforcing a centralized worldview.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, imperial_court, beneficiary,
    institutional, generational, constrained, national).

% Patrons of both kami shrines and Buddhist temples, they benefit from the social cohesion and spiritual authority derived from the syncretic system. Their local power is often reinforced by their association with integrated religious institutions.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_elites, beneficiary,
    powerful, biographical, mobile, regional).

% Their indigenous traditions and unique kami identities are subsumed under the universalizing Buddhist framework. While gaining access to Buddhist ritual and patronage, they lose autonomy and distinctiveness, often seeing their kami reinterpreted as bodhisattvas or other Buddhist figures.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_kami_cults, payer,
    powerless, generational, identity_locked, local).

% Individuals whose religious practices might not align with the syncretic framework. They are subtly pressured to conform to the dominant interpretation, with alternatives being marginalized or deemed less legitimate within the prevailing religious landscape.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, unaffiliated_practitioners, payer,
    powerless, biographical, constrained, local).

% A later historical actor that would actively dismantle this syncretic system through shinbutsu bunri (separation of kami and Buddhas), viewing it as an impediment to national unity and a pure 'Shinto' identity. Their perspective is excluded from the internal logic of the syncretic system itself.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, meiji_restoration_government, excluded,
    institutional, generational, analytical, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent theological framework that integrates diverse local kami traditions into a universal Buddhist cosmology, facilitating religious unity and institutional cooperation across Japan.
% TRANSFER_FUNCTION: Transfers spiritual authority and interpretive control from local kami traditions to the Buddhist establishment, while also channeling patronage and landholdings to integrated shrine-temple complexes.
% ABSENT_VOICES: The Meiji Restoration government, which later forcibly separated kami and Buddhas, would argue that the syncretic fusion was an artificial construct that obscured a 'pure' Shinto identity. Their voice is absent from the pre-Meiji discourse that sustained honji suijaku.
% DISAPPEARANCE_RATIONALE: If the honji suijaku doctrine vanished, the entire religious landscape of pre-Meiji Japan would fundamentally reorganize. Jinguji complexes would dissolve, Buddhist institutions would lose significant land and patronage, and local kami traditions would either reassert independent identities or face new forms of integration, leading to widespread institutional and theological upheaval.
% FOUNDING_PROBLEM: To reconcile the indigenous kami worship with the newly introduced, universalizing Buddhist doctrines, providing a theological basis for their coexistence and integration within a single religious system.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist scholars and historical texts from the Heian and Kamakura periods attest to the theological and social necessity of integrating kami into the Buddhist framework to facilitate its spread and acceptance in Japan. This is corroborated by the widespread establishment of jinguji and the textual production of honji suijaku theories.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__syncretic_fusion_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).
:- end_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.35) and suppression (0.45) are moderate. While the doctrine provided a unifying framework, it did so by reinterpreting local kami traditions, which involved a loss of autonomy and distinctiveness for indigenous cults. The 'theater ratio' is low (0.1) because the theological work of integration was genuine and actively maintained by the Buddhist clergy, not merely performative. Accessibility collapse is high (0.7) because this reading became the dominant, almost unquestioned, framework for understanding kami-Buddha relations, making alternative interpretations difficult to sustain within the mainstream.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Buddhist clergy and imperial court, honji suijaku was a successful and beneficial coordination mechanism. From the perspective of local kami cults, it involved a degree of subsumption and loss of original identity, even if it offered new forms of patronage and legitimacy. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Buddhist clergy, as the primary architects and interpreters of honji suijaku, are clear beneficiaries and agenda-setters. The Imperial Court and local elites also benefit from the ideological and political stability it provides. Local kami cults and unaffiliated practitioners are payers, as their traditions are reinterpreted and subsumed, leading to a loss of independent identity and authority. Their exit options are identity_locked, as their spiritual practices are deeply intertwined with the syncretic framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    theological_coherence_vs_power_dynamics,
    'To what extent was honji suijaku a genuine theological synthesis versus a strategic move by the Buddhist establishment to absorb indigenous traditions and consolidate power?',
    'Detailed historical-theological analysis of primary texts, focusing on the internal consistency of arguments versus the institutional outcomes (e.g., land acquisition, patronage shifts).',
    'If primarily strategic, the extractiveness and suppression metrics for the Buddhist clergy would be higher, reclassifying the constraint closer to a Tangled Rope or Snare from the payer seats. If genuine synthesis, the Rope classification holds more strongly.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(theological_coherence_vs_power_dynamics, conceptual, 'Ambiguity between theological synthesis and power consolidation.').

omega_variable(
    identity_lock_depth,
    'How deeply internalized was the honji suijaku identity for local kami cults? Would they have genuinely preferred a separate, un-Buddhified identity if given a free choice, or had the fusion become their authentic self-understanding?',
    'Analysis of local shrine records and folk practices for evidence of resistance or alternative interpretations, particularly in periods of weaker central control. Post-Meiji shinbutsu bunri responses could also offer insight into pre-existing identity structures.',
    'If the identity lock was superficial, the suppression metric for local kami cults would be lower, and their exit options would be closer to ''constrained'' than ''identity_locked''. If deeply internalized, the ''identity_locked'' assessment holds, indicating a more profound form of constraint.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(identity_lock_depth, empirical, 'Depth of identity fusion for local kami cults.').

omega_variable(
    syncretic_fusion_vs_incoherent_bundle,
    'Was honji suijaku, as understood by this reading, a truly coherent ontological fusion, or was it always an ''incoherent bundle'' of practices and beliefs held together by institutional power and deliberate ambiguity, as argued by the ''incoherent_bundle_reading''?',
    'Comparative textual analysis of theological treatises and popular religious practices across different periods and regions, seeking evidence of consistent application of the fusion principle versus pragmatic, context-dependent interpretations.',
    'If the ''incoherent_bundle_reading'' is more accurate, this constraint''s claimed ''rope'' type would be challenged, and its extractiveness and theater_ratio would likely be higher, as the ''coordination'' function would be revealed as a cover for maintaining an unstable, power-driven arrangement. This reading would then be foreclosed by the incoherent_bundle_reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(syncretic_fusion_vs_incoherent_bundle, conceptual, 'Coherence of the syncretic fusion.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 794, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t794, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 794, 0.05).
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1000, 0.08).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1200, 0.1).
narrative_ontology:measurement(shin_tr_t1400, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1400, 0.12).
narrative_ontology:measurement(shin_tr_t1600, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1600, 0.11).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1868, 0.1).

% Extraction over time
narrative_ontology:measurement(shin_be_t794, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 794, 0.25).
narrative_ontology:measurement(shin_be_t1000, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1000, 0.3).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1200, 0.35).
narrative_ontology:measurement(shin_be_t1400, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1400, 0.38).
narrative_ontology:measurement(shin_be_t1600, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1600, 0.37).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1868, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t794, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 794, 0.3).
narrative_ontology:measurement(shin_su_t1000, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1000, 0.38).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1200, 0.45).
narrative_ontology:measurement(shin_su_t1400, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1400, 0.48).
narrative_ontology:measurement(shin_su_t1600, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1600, 0.47).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1868, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__syncretic_fusion_reading, identity_coordination).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'shinbutsu_coexistence_commitment' kernel. This 'syncretic_fusion_reading' emphasizes ontological unification through honji suijaku. It contrasts with the 'domain_partition_reading' (separate domains) and the 'incoherent_bundle_reading' (lack of coherence).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
