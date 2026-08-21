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
 *   constraint_id: kami_buddha_ontology__honji_suijaku_monism
 *   human_readable: Honji Suijaku Monism: Kami as Traces of Buddhas
 *   domain: religious_studies/philosophy_of_religion/japanese_cultural_history
 *
 * SUMMARY:
 *   This constraint describes the 'honji suijaku' (original ground and trace
 *   manifestation) theory, a dominant theological framework in pre-modern
 *   Japan that posited kami (indigenous Japanese deities) as phenomenal
 *   manifestations (suijaku) of original Buddhist entities (honji). This
 *   reading establishes an ontological monism where Buddhist figures are the
 *   ultimate reality, and kami are their local, provisional traces. This
 *   story instantiates the 'honji_suijaku_monism' reading of the
 *   'kami_buddha_ontology' kernel.
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
narrative_ontology:cs_story_uid(kami_buddha_ontology__honji_suijaku_monism, '828ed58e-2a70-4fa8-941c-4e860273b5b9').
narrative_ontology:cs_kernel_codification('828ed58e-2a70-4fa8-941c-4e860273b5b9', formalized).
narrative_ontology:cs_authority_grounding('828ed58e-2a70-4fa8-941c-4e860273b5b9', lineage).
narrative_ontology:cs_interpretation_layer_present('828ed58e-2a70-4fa8-941c-4e860273b5b9').
narrative_ontology:cs_reading_relation('828ed58e-2a70-4fa8-941c-4e860273b5b9', kami_buddha_ontology__domain_partition, coexists_with).
narrative_ontology:cs_reading_relation('828ed58e-2a70-4fa8-941c-4e860273b5b9', kami_buddha_ontology__incoherent_bundle, coexists_with).
narrative_ontology:cs_axiom('828ed58e-2a70-4fa8-941c-4e860273b5b9', foundational, buddhist_entities_as_ultimate_reality).
narrative_ontology:cs_axiom_status(buddhist_entities_as_ultimate_reality, holdable).
narrative_ontology:cs_axiom_grounding('828ed58e-2a70-4fa8-941c-4e860273b5b9', buddhist_entities_as_ultimate_reality, theological).
narrative_ontology:cs_axiom('828ed58e-2a70-4fa8-941c-4e860273b5b9', foundational, kami_as_provisional_manifestations).
narrative_ontology:cs_axiom_status(kami_as_provisional_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('828ed58e-2a70-4fa8-941c-4e860273b5b9', kami_as_provisional_manifestations, theological).
narrative_ontology:cs_reference_frame('828ed58e-2a70-4fa8-941c-4e860273b5b9', buddhist_cosmological_supremacy).
narrative_ontology:cs_drift_state('828ed58e-2a70-4fa8-941c-4e860273b5b9', contemporary_post_shinbutsu_bunri_era, gap(repudiation_pressure, substantial, true)).
narrative_ontology:cs_created_at('828ed58e-2a70-4fa8-941c-4e860273b5b9', '2024-07-30T12:00:00Z').
narrative_ontology:cs_kernel_id(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, buddhist_scholars).
narrative_ontology:constraint_beneficiary(kami_buddha_ontology__honji_suijaku_monism, syncretic_religious_institutions).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, shinto_priests).
narrative_ontology:constraint_victim(kami_buddha_ontology__honji_suijaku_monism, indigenous_kami_worshippers).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, buddhist_universalism).
narrative_ontology:constraint_vindicates(kami_buddha_ontology__honji_suijaku_monism, hierarchical_ontology).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary proponents and systematizers of the honji suijaku theory, benefiting from the intellectual coherence and hierarchical order it provides within their theological framework. They actively interpret and transmit this understanding.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, buddhist_scholars, agenda_setter,
    institutional, generational, mobile, national).

% Religious organizations that integrate both Buddhist and Shinto elements, finding a theological justification for their practices in honji suijaku monism. This doctrine provides a stable framework for their syncretic identity.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, syncretic_religious_institutions, beneficiary,
    organized, generational, constrained, local).

% While some Shinto priests adopted this framework, it often placed kami in a subordinate ontological position, potentially diminishing the independent authority of Shinto traditions. They bear the cost of theological subsumption.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, shinto_priests, payer,
    moderate, biographical, constrained, local).

% For those whose primary religious identity is tied to local kami worship, this doctrine can be experienced as an imposition that redefines their deities through a foreign lens, even if it allows for continued practice. Their identity is deeply intertwined with the local kami.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, indigenous_kami_worshippers, payer,
    powerless, biographical, identity_locked, local).

% Advocates for a 'pure' Shinto that rejects Buddhist influence and the honji suijaku framework. They are excluded from the dominant theological discourse that accepts this monistic view, actively seeking to re-establish an independent Shinto ontology.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, modern_shinto_revivalists, excluded,
    organized, generational, mobile, national).

% Analyze the philosophical implications and historical development of honji suijaku monism, evaluating its coherence, consistency, and impact on religious practice and identity. They are external to the direct religious practice but influence academic discourse.
narrative_ontology:constraint_stakeholder(kami_buddha_ontology__honji_suijaku_monism, philosophers_of_religion, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(kami_buddha_ontology__honji_suijaku_monism, buddhist_scholars).
narrative_ontology:fixing_cost_class(kami_buddha_ontology__honji_suijaku_monism, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a coherent theological framework for the integration of indigenous Japanese kami worship with the universalistic doctrines of Buddhism, allowing for a unified religious landscape and shared ritual practices.
% TRANSFER_FUNCTION: Transfers ontological priority and explanatory power from indigenous kami to Buddhist entities, while allowing kami to retain phenomenal presence and ritual function. It systematizes diverse local beliefs under a dominant Buddhist cosmology.
% ABSENT_VOICES: Modern Shinto revivalists and scholars advocating for the independent ontological status of kami are structurally excluded from the discourse that accepts honji suijaku as a foundational truth. They would argue for a distinct, non-subordinate Shinto identity.
% DISAPPEARANCE_RATIONALE: If honji suijaku monism vanished, the theological justification for many syncretic institutions would collapse, leading to a re-evaluation of the relationship between kami and buddhas. This would likely result in a more pronounced separation of Shinto and Buddhist practices, or the emergence of new, distinct syncretic theories.
% FOUNDING_PROBLEM: The problem of reconciling indigenous Japanese kami worship with the arrival and spread of Buddhism, which presented a universalistic cosmology that needed to account for local deities.
% FOUNDING_PROBLEM_CORROBORATION: Buddhist scholars and syncretic institutions attest that the problem of integrating diverse religious beliefs remains live, requiring a coherent theological framework. Modern Shinto revivalists acknowledge the historical problem but contest the honji suijaku solution, arguing for an alternative approach to inter-religious relations.
narrative_ontology:disappearance_verdict(kami_buddha_ontology__honji_suijaku_monism, world_rearranges).
narrative_ontology:founding_problem_status(kami_buddha_ontology__honji_suijaku_monism, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(kami_buddha_ontology__honji_suijaku_monism, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
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
 *   The constraint is classified as a 'rope' because it provided a widely accepted, coherent framework for religious syncretism, benefiting many institutions and individuals by resolving a significant theological tension. Extractiveness is low (0.25) because it allowed for the continued veneration of kami, rather than outright suppression. Suppression (0.35) exists in the sense that alternative, non-subordinate ontologies for kami were marginalized, but not violently suppressed. Theater ratio is low (0.1) as the doctrine served a genuine theological and social function. Accessibility collapse (0.7) is high because once this framework was adopted, it became difficult to conceive of kami and buddhas as entirely separate entities within the dominant discourse. Resistance (0.15) was low during its peak influence, as it offered a workable solution for many.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Buddhist scholars, this is a highly effective 'rope' that brings order and meaning. From the perspective of indigenous kami worshippers, it might feel more like a 'tangled_rope' or even a 'snare' that subtly diminishes their traditions, even if it allows for their continuation. The engine's classification will reflect this divergence based on the structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Buddhist scholars and syncretic institutions are beneficiaries, gaining intellectual coherence and institutional legitimacy. Shinto priests and indigenous kami worshippers are payers, as their traditions are ontologically subsumed, even if their practices continue. Modern Shinto revivalists are excluded, as their alternative ontological claims are outside the accepted framework. Philosophers of religion act as observers, analyzing the system without direct participation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kami_buddha_ontology_kernel_reading,
    'Is this constraint a true ontological description, or a theological interpretation that serves institutional interests?',
    'Analysis of historical texts and contemporary religious practice, focusing on the lived experience of adherents versus scholarly systematization.',
    'If primarily an interpretation, its classification might shift towards a ''tangled_rope'' for those whose indigenous beliefs are subsumed, or a ''snare'' if actively enforced to suppress alternative views. If a true description, it remains a ''mountain'' or ''rope'' of understanding.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kami_buddha_ontology_kernel_reading, conceptual, 'This constraint is one reading (honji_suijaku_monism) of the kami_buddha_ontology kernel. Sibling readings (domain_partition, incoherent_bundle) offer alternative structural relationships between kami and buddhas.').

omega_variable(
    institutional_vs_theological_grounding,
    'To what extent is the persistence of honji suijaku monism driven by its theological coherence versus its utility in consolidating Buddhist institutional power?',
    'Historical analysis of periods of institutional competition between Shinto and Buddhist establishments, and the role of this doctrine in legitimizing Buddhist dominance.',
    'If primarily institutionally driven, the ''extractiveness'' and ''suppression'' metrics might be higher than currently assessed, reflecting the costs borne by indigenous Shinto practices or alternative interpretations. This would push the classification towards ''tangled_rope'' or ''snare''.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(institutional_vs_theological_grounding, empirical, 'Ambiguity regarding the primary driver of the doctrine''s persistence.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(kami_buddha_ontology__honji_suijaku_monism, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(kami_tr_t0, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 0, 0.05).
narrative_ontology:measurement(kami_tr_t100, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 100, 0.08).
narrative_ontology:measurement(kami_tr_t200, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 200, 0.09).
narrative_ontology:measurement(kami_tr_t300, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 300, 0.1).
narrative_ontology:measurement(kami_tr_t400, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 400, 0.09).
narrative_ontology:measurement(kami_tr_t500, kami_buddha_ontology__honji_suijaku_monism, theater_ratio, 500, 0.1).

% Extraction over time
narrative_ontology:measurement(kami_be_t0, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 0, 0.2).
narrative_ontology:measurement(kami_be_t100, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 100, 0.25).
narrative_ontology:measurement(kami_be_t200, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 200, 0.23).
narrative_ontology:measurement(kami_be_t300, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 300, 0.25).
narrative_ontology:measurement(kami_be_t400, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 400, 0.24).
narrative_ontology:measurement(kami_be_t500, kami_buddha_ontology__honji_suijaku_monism, base_extractiveness, 500, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(kami_su_t0, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 0, 0.3).
narrative_ontology:measurement(kami_su_t100, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 100, 0.32).
narrative_ontology:measurement(kami_su_t200, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 200, 0.33).
narrative_ontology:measurement(kami_su_t300, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 300, 0.35).
narrative_ontology:measurement(kami_su_t400, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 400, 0.34).
narrative_ontology:measurement(kami_su_t500, kami_buddha_ontology__honji_suijaku_monism, suppression_requirement, 500, 0.35).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(kami_buddha_ontology__honji_suijaku_monism, identity_coordination).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__domain_partition).
narrative_ontology:affects_constraint(kami_buddha_ontology__honji_suijaku_monism, kami_buddha_ontology__incoherent_bundle).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the kami_buddha_ontology kernel. Its ε value reflects the specific ontological claim of kami as traces of buddhas, distinct from readings that posit separate domains or an incoherent bundle of commitments.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
