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
 *   human_readable: Honji Suijaku: Kami as Buddhist Manifestations (Syncretic Fusion Reading)
 *   domain: religious_studies/philosophy_of_religion/japanese_history
 *
 * SUMMARY:
 *   This constraint story describes the 'syncretic fusion' reading of the
 *   shinbutsu coexistence commitment, where indigenous kami were understood
 *   as local manifestations of universal Buddhist truth (honji suijaku). This
 *   reading provided a coherent theological framework for integrating diverse
 *   religious practices, but also served to subordinate kami cults and
 *   consolidate Buddhist institutional power. The metrics reflect a long
 *   historical arc from the Heian period to the Meiji Restoration, showing
 *   increasing extractiveness and suppression as the doctrine became more
 *   entrenched and its institutional benefits solidified.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.75).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.85).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.45).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, extractiveness, 0.75).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0.45).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "Honji Suijaku: Kami as Buddhist Manifestations (Syncretic Fusion Reading)").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "religious_studies/philosophy_of_religion/japanese_history").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '0c6d4b77-1089-413c-93d0-8f05d984d520').
narrative_ontology:cs_kernel_codification('0c6d4b77-1089-413c-93d0-8f05d984d520', formalized).
narrative_ontology:cs_authority_grounding('0c6d4b77-1089-413c-93d0-8f05d984d520', lineage).
narrative_ontology:cs_interpretation_layer_present('0c6d4b77-1089-413c-93d0-8f05d984d520').
narrative_ontology:cs_reading_relation('0c6d4b77-1089-413c-93d0-8f05d984d520', shinbutsu_coexistence_commitment__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('0c6d4b77-1089-413c-93d0-8f05d984d520', shinbutsu_coexistence_commitment__incoherent_bundle_reading, forecloses).
narrative_ontology:cs_axiom('0c6d4b77-1089-413c-93d0-8f05d984d520', foundational, kami_are_buddhist_manifestations).
narrative_ontology:cs_axiom_status(kami_are_buddhist_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('0c6d4b77-1089-413c-93d0-8f05d984d520', kami_are_buddhist_manifestations, theological).
narrative_ontology:cs_axiom('0c6d4b77-1089-413c-93d0-8f05d984d520', foundational, universal_buddhist_truth_underlies_local_kami).
narrative_ontology:cs_axiom_status(universal_buddhist_truth_underlies_local_kami, holdable).
narrative_ontology:cs_axiom_grounding('0c6d4b77-1089-413c-93d0-8f05d984d520', universal_buddhist_truth_underlies_local_kami, theological).
narrative_ontology:cs_reference_frame('0c6d4b77-1089-413c-93d0-8f05d984d520', heian_syncretic_order).
narrative_ontology:cs_drift_state('0c6d4b77-1089-413c-93d0-8f05d984d520', meiji_restoration_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('0c6d4b77-1089-413c-93d0-8f05d984d520', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theological_elite).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, imperial_court).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_kami_cults).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinto_priests_without_buddhist_ties).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, common_worshippers).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Actively promoted and enforced the honji suijaku doctrine, integrating kami cults into their temples (jinguji) and consolidating spiritual and material authority. Benefited from increased patronage and landholdings.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_institutions, agenda_setter,
    institutional, generational, arbitrage, national).

% Developed and refined the complex theological arguments for honji suijaku, gaining intellectual prestige and influence within the religious hierarchy. Their careers and authority were tied to the doctrine's dominance.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theological_elite, beneficiary,
    powerful, biographical, constrained, national).

% Supported the syncretic framework as a means of religious and political stability, integrating diverse local beliefs under a unified (Buddhist-led) religious order. Provided patronage and official sanction.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, imperial_court, beneficiary,
    institutional, generational, mobile, national).

% Often found their indigenous kami reinterpreted as local manifestations of Buddhist deities, leading to a loss of independent identity, ritual autonomy, and sometimes material resources to Buddhist temples. Their worship continued, but under a new interpretive frame.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, local_kami_cults, payer,
    powerless, biographical, identity_locked, local).

% Those who wished to maintain a purely Shinto identity and practice faced pressure to conform to the syncretic model, risking marginalization or loss of patronage if they resisted the integration with Buddhist institutions.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinto_priests_without_buddhist_ties, payer,
    moderate, biographical, constrained, local).

% Benefited from a unified and accessible religious landscape, allowing them to worship both kami and Buddhas without perceived contradiction, often at the same sites. Experienced a coherent spiritual worldview.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, common_worshippers, beneficiary,
    powerless, immediate, constrained, local).

% Analyze the historical development, theological implications, and institutional effects of honji suijaku, often from a critical distance, identifying its coordination and extractive functions.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, modern_scholars, observer,
    analytical, biographical, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provided a coherent theological framework for the coexistence and integration of indigenous kami worship with imported Buddhist doctrines, reducing religious conflict and facilitating shared ritual practice across Japan.
% TRANSFER_FUNCTION: Transferred spiritual authority, interpretive control, and material resources (e.g., land, patronage) from independent kami cults to Buddhist temples and their associated jinguji (shrine-temples), consolidating Buddhist institutional power.
% ABSENT_VOICES: Pure Shinto revivalists, particularly Kokugaku scholars from the Edo period, who emphasized kami independence and rejected Buddhist subordination, were structurally marginalized. They would have argued for a distinct, non-syncretic Shinto identity.
% DISAPPEARANCE_RATIONALE: If the honji suijaku doctrine and its institutional embodiment (jinguji) had vanished overnight before the Meiji Restoration, the entire religious landscape of pre-Meiji Japan would have been fundamentally different, likely leading to greater fragmentation, conflict, or a different form of syncretism. The theological justifications for many practices and the institutional structures would collapse.
% FOUNDING_PROBLEM: The need to reconcile indigenous Japanese kami worship with the powerful, universalizing Buddhist tradition introduced from the continent, avoiding conflict and integrating diverse religious practices into a cohesive social and political order.
% FOUNDING_PROBLEM_CORROBORATION: Historical records, archaeological evidence of jinguji structures, and analyses by modern historians of Japanese religion corroborate the problem and the doctrine's role. While the doctrine was officially dismantled during the Meiji Restoration, its legacy persists, but the original 'problem' of reconciling two active, competing traditions in a pre-modern context is no longer live.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__syncretic_fusion_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.75, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(shinbutsu_coexistence_commitment__syncretic_fusion_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.75) and suppression (0.85) reflect the historical reality that while honji suijaku offered a theological synthesis, it often led to the institutional and doctrinal subordination of Shinto elements to Buddhist temples. The 'tangled_rope' classification captures both the genuine coordination function (religious coherence) and the asymmetric extraction. Theater ratio (0.45) indicates that while the theological framework was sophisticated, some aspects of its maintenance became performative, defending institutional arrangements rather than purely spiritual synthesis. Accessibility collapse is high (0.8) because for those within the dominant framework, alternative interpretations of kami independence were largely foreclosed.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Buddhist institutions, honji suijaku was a sophisticated theological solution to religious diversity, a 'rope' of profound spiritual coordination. From the perspective of marginalized Shinto elements, it was a 'snare' that absorbed and subordinated their traditions. The engine's computation of per-seat classification from the structural data will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist institutions and the theological elite were primary beneficiaries, gaining authority and resources. The Imperial Court also benefited from religious stability. Local kami cults and Shinto priests without strong Buddhist ties were the primary targets, experiencing a loss of autonomy and identity. Common worshippers received coordination benefits (unified practice) but also bore indirect costs of institutional consolidation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    domain_partition_ambiguity,
    'Is the honji suijaku doctrine''s unification truly ontological, or was it primarily a functional division of labor between kami and Buddhas that only appeared unified?',
    'Detailed textual analysis of primary sources from different periods and regions, focusing on the lived religious experience and local interpretations rather than official doctrine.',
    'If primarily functional, the ''domain_partition_reading'' gains strength, suggesting less ontological fusion and potentially lower inherent suppression of kami independence. If truly ontological, this ''syncretic_fusion_reading'' is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(domain_partition_ambiguity, conceptual, 'Ambiguity between ontological fusion and functional division in shinbutsu syncretism.').

omega_variable(
    incoherence_vs_coherence,
    'Was the honji suijaku framework a genuinely coherent theological system, or an ''incoherent bundle'' of pragmatic accommodations maintained through deliberate ambiguity and institutional power?',
    'Comparative analysis of theological treatises with actual ritual practices and local cult dynamics, seeking internal contradictions or explicit strategic ambiguities in the doctrine''s application.',
    'If incoherent, the ''incoherent_bundle_reading'' is strengthened, suggesting the constraint''s persistence relied more on institutional power and less on its inherent theological ''truth'', potentially increasing its effective extraction and theater ratio.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(incoherence_vs_coherence, empirical, 'Whether shinbutsu syncretism was a coherent system or a pragmatic, ambiguous bundle.').

omega_variable(
    subordination_vs_integration,
    'To what extent was honji suijaku a genuine integration of kami into a broader Buddhist worldview, versus a mechanism for the institutional subordination and absorption of kami cults?',
    'Socio-economic analysis of resource flows and power dynamics between Buddhist temples and kami shrines, alongside theological analysis of the degree of interpretive flexibility allowed for kami traditions.',
    'If primarily subordination, the extractive component of this constraint is amplified, and the coordination function is diminished. If genuine integration, the coordination function is stronger, and extraction is seen more as a cost of synthesis.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(subordination_vs_integration, empirical, 'The balance between genuine integration and institutional subordination in honji suijaku.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 800, 1868).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t800, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 800, 0.2).
narrative_ontology:measurement(shin_tr_t1000, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1000, 0.25).
narrative_ontology:measurement(shin_tr_t1200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1200, 0.3).
narrative_ontology:measurement(shin_tr_t1400, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1400, 0.35).
narrative_ontology:measurement(shin_tr_t1600, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1600, 0.4).
narrative_ontology:measurement(shin_tr_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 1868, 0.45).

% Extraction over time
narrative_ontology:measurement(shin_be_t800, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 800, 0.5).
narrative_ontology:measurement(shin_be_t1000, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1000, 0.6).
narrative_ontology:measurement(shin_be_t1200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1200, 0.65).
narrative_ontology:measurement(shin_be_t1400, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1400, 0.7).
narrative_ontology:measurement(shin_be_t1600, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1600, 0.72).
narrative_ontology:measurement(shin_be_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 1868, 0.75).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t800, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 800, 0.6).
narrative_ontology:measurement(shin_su_t1000, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1000, 0.65).
narrative_ontology:measurement(shin_su_t1200, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1200, 0.7).
narrative_ontology:measurement(shin_su_t1400, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1400, 0.75).
narrative_ontology:measurement(shin_su_t1600, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1600, 0.8).
narrative_ontology:measurement(shin_su_t1868, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 1868, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__syncretic_fusion_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__domain_partition_reading).
narrative_ontology:affects_constraint(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment__incoherent_bundle_reading).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'shinbutsu_coexistence_commitment' kernel. It represents the 'syncretic fusion' interpretation, where kami are seen as Buddhist manifestations. It is linked to sibling readings that offer alternative interpretations of the kami-Buddha relationship.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
