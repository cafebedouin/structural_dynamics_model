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
 *   constraint_id: shinbutsu_coexistence_commitment__syncretic_fusion_reading
 *   human_readable: Honji Suijaku Syncretic Fusion Doctrine
 *   domain: religious_studies/philosophy_of_religion/japanese_history
 *
 * SUMMARY:
 *   This constraint describes the theological and institutional framework of
 *   Honji Suijaku, where indigenous Kami are understood as local
 *   manifestations of universal Buddhist truths. This reading asserts a
 *   coherent, unified ontology that integrated Japanese religious life for
 *   centuries. The constraint is claimed as a Rope by its proponents (a
 *   beneficial coordination of diverse spiritual paths) but operates with
 *   substantial extraction and suppression, particularly from those whose
 *   traditions are subsumed. This story is one reading of the
 *   'shinbutsu_coexistence_commitment' kernel, focusing on the syncretic
 *   fusion perspective.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.65).
domain_priors:suppression_score(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.75).
domain_priors:theater_ratio(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.4).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0.4).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(shinbutsu_coexistence_commitment__syncretic_fusion_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, tangled_rope).
narrative_ontology:human_readable(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "Honji Suijaku Syncretic Fusion Doctrine").
narrative_ontology:topic_domain(shinbutsu_coexistence_commitment__syncretic_fusion_reading, "religious_studies/philosophy_of_religion/japanese_history").

domain_priors:requires_active_enforcement(shinbutsu_coexistence_commitment__syncretic_fusion_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '55c77455-7d4a-41f6-b889-ada7d1d09e9e').
narrative_ontology:cs_kernel_codification('55c77455-7d4a-41f6-b889-ada7d1d09e9e', formalized).
narrative_ontology:cs_authority_grounding('55c77455-7d4a-41f6-b889-ada7d1d09e9e', lineage).
narrative_ontology:cs_interpretation_layer_present('55c77455-7d4a-41f6-b889-ada7d1d09e9e').
narrative_ontology:cs_reading_relation('55c77455-7d4a-41f6-b889-ada7d1d09e9e', shinbutsu_coexistence_commitment__domain_partition_reading, forecloses).
narrative_ontology:cs_reading_relation('55c77455-7d4a-41f6-b889-ada7d1d09e9e', shinbutsu_coexistence_commitment__incoherent_bundle_reading, forecloses).
narrative_ontology:cs_axiom('55c77455-7d4a-41f6-b889-ada7d1d09e9e', foundational, kami_as_buddha_manifestations).
narrative_ontology:cs_axiom_status(kami_as_buddha_manifestations, holdable).
narrative_ontology:cs_axiom_grounding('55c77455-7d4a-41f6-b889-ada7d1d09e9e', kami_as_buddha_manifestations, theological).
narrative_ontology:cs_axiom('55c77455-7d4a-41f6-b889-ada7d1d09e9e', foundational, ontological_unity_of_divine).
narrative_ontology:cs_axiom_status(ontological_unity_of_divine, holdable).
narrative_ontology:cs_axiom_grounding('55c77455-7d4a-41f6-b889-ada7d1d09e9e', ontological_unity_of_divine, deontological).
narrative_ontology:cs_reference_frame('55c77455-7d4a-41f6-b889-ada7d1d09e9e', unified_buddhist_shinto_ontology).
narrative_ontology:cs_drift_state('55c77455-7d4a-41f6-b889-ada7d1d09e9e', meiji_restoration_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('55c77455-7d4a-41f6-b889-ada7d1d09e9e', '').
narrative_ontology:cs_kernel_id(shinbutsu_coexistence_commitment__syncretic_fusion_reading, shinbutsu_coexistence_commitment).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_theological_elite).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_institutions).
narrative_ontology:constraint_beneficiary(shinbutsu_coexistence_commitment__syncretic_fusion_reading, imperial_court_aristocracy).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kami_worshippers_outside_buddhist_framework).
narrative_ontology:constraint_victim(shinbutsu_coexistence_commitment__syncretic_fusion_reading, native_shinto_priests).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Develop and propagate the honji suijaku doctrine, maintaining its intellectual coherence and institutional dominance. Their authority and intellectual careers are deeply intertwined with the doctrine's acceptance.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_theological_elite, agenda_setter,
    institutional, generational, identity_locked, national).

% Benefit from the expanded legitimacy, patronage, and landholdings derived from incorporating Kami worship, often managing jinguji (temple-shrines) where Kami are enshrined alongside Buddhas.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_institutions, beneficiary,
    institutional, generational, constrained, national).

% Their traditional, independent worship of Kami is subsumed or reinterpreted through a Buddhist lens, potentially losing autonomy or distinct identity. They are expected to accept the syncretic framework.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, kami_worshippers_outside_buddhist_framework, payer,
    powerless, biographical, constrained, local).

% Their authority and distinct religious tradition are challenged or subordinated by the syncretic framework, requiring them to adapt, integrate, or resist. Their professional identity is often tied to the purity of Shinto.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, native_shinto_priests, payer,
    organized, biographical, identity_locked, regional).

% Benefits from a unified religious ideology that supports state legitimacy and social cohesion. They often patronize both Buddhist temples and Shinto shrines, reinforcing the syncretic framework as a state-sanctioned orthodoxy.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, imperial_court_aristocracy, beneficiary,
    powerful, generational, constrained, national).

% Analyze the historical development, theological implications, and social impact of honji suijaku, assessing its coherence and role in Japanese religious history from an academic perspective.
narrative_ontology:constraint_stakeholder(shinbutsu_coexistence_commitment__syncretic_fusion_reading, scholarly_observers, observer,
    analytical, biographical, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(shinbutsu_coexistence_commitment__syncretic_fusion_reading, buddhist_institutions).
narrative_ontology:fixing_cost_class(shinbutsu_coexistence_commitment__syncretic_fusion_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a unified theological framework for understanding the relationship between indigenous Kami and imported Buddhist deities, preventing religious conflict and integrating diverse spiritual practices into a coherent worldview.
% TRANSFER_FUNCTION: Transfers theological authority, institutional patronage, and interpretive control towards Buddhist institutions and their interpretive elite, while reinterpreting or subsuming indigenous Kami traditions and their independent priesthoods.
% ABSENT_VOICES: Early native Shinto proponents who resisted Buddhist assimilation, or later Kokugaku scholars who sought to purify Shinto from Buddhist influence. They would argue for the distinctness and supremacy of Kami, rejecting the syncretic fusion.
% DISAPPEARANCE_RATIONALE: If the honji suijaku doctrine and its enforcement vanished overnight, the religious landscape of pre-Meiji Japan would be fundamentally altered. The institutional structures (jinguji), theological justifications for state power, and the daily practices of many would lose their coherence, leading to a re-evaluation of Kami and Buddhist roles and a potential resurgence of distinct Shinto traditions.
% FOUNDING_PROBLEM: To reconcile the indigenous Japanese reverence for Kami with the growing influence and universal claims of Buddhism, avoiding religious schism and integrating new spiritual practices into the existing cultural fabric without outright rejection of either tradition.
% FOUNDING_PROBLEM_CORROBORATION: Historical texts from both Buddhist and Shinto traditions, as well as modern religious studies scholars, corroborate the historical need for such a reconciliation. However, the extent to which the 'problem' was truly solved or merely managed through a dominant framework is contested, especially in light of later anti-syncretic movements.
narrative_ontology:disappearance_verdict(shinbutsu_coexistence_commitment__syncretic_fusion_reading, world_rearranges).
narrative_ontology:founding_problem_status(shinbutsu_coexistence_commitment__syncretic_fusion_reading, contested).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 'none', 1).
narrative_ontology:epsilon_provenance(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

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
 *   Extraction is high (0.65) due to the intellectual and institutional demands placed on non-Buddhist traditions, requiring reinterpretation or subordination. Suppression is also high (0.75) as the theological elite actively enforced doctrinal consistency and institutional dominance, limiting alternative interpretations. Theater ratio is moderate (0.40): while genuine theological work and spiritual practice occurred, a significant portion of institutional activity served to maintain the syncretic framework's power and legitimacy. The measurement series reflects the gradual entrenchment of this doctrine over a long historical period.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Buddhist theological elite, this doctrine was a sophisticated and beneficial coordination mechanism, integrating diverse spiritual paths. From the perspective of native Shinto priests and Kami worshippers, it represented a form of cultural and religious subsumption, where their traditions were reinterpreted and their autonomy diminished. The engine's per-seat classification will highlight this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Buddhist theological elite and institutions are clear beneficiaries, gaining authority, patronage, and an expanded religious domain. The imperial court also benefits from a unified religious ideology. Native Shinto priests and Kami worshippers are targets, as their traditions are reinterpreted or subordinated, and they bear the costs of maintaining the syncretic framework. Their exit options are constrained by deep cultural and identity ties.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    kernel_identity_ambiguity,
    'Is the relationship between Kami and Buddhist deities fundamentally one of ontological fusion (this reading), domain partition, or an incoherent bundle?',
    'Analysis of primary historical texts, archaeological evidence of religious practice, and the internal consistency of various theological arguments. The Meiji Restoration''s forced separation provides a historical ''natural experiment'' on the coherence of the fusion.',
    'If the ''domain_partition_reading'' or ''incoherent_bundle_reading'' were structurally true, the classification of this constraint would shift dramatically, likely towards a Snare (if incoherent and maintained by power) or a Rope (if genuinely partitioned and coordinating separate domains).',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_identity_ambiguity, conceptual, 'Ambiguity regarding the fundamental ontological relationship between Kami and Buddhist deities.').

omega_variable(
    doctrinal_coherence_vs_power,
    'To what extent was the syncretic fusion a genuinely coherent theological system, versus a framework whose persistence was primarily due to institutional power and suppression of alternatives?',
    'Detailed textual analysis of internal theological debates and contradictions within the honji suijaku framework, alongside historical evidence of institutional enforcement and suppression of dissenting views.',
    'If the coherence was largely superficial and maintained by power, the constraint''s effective extraction would be higher, and its classification would lean more strongly towards a Snare, even if claimed as a Rope.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(doctrinal_coherence_vs_power, empirical, 'Whether the syncretic doctrine''s coherence was genuine or power-enforced.').

omega_variable(
    historical_agency_of_kami_worshippers,
    'How much agency did indigenous Kami worshippers and native Shinto priests retain in shaping the syncretic doctrine, versus being passively subsumed by the dominant Buddhist framework?',
    'Micro-historical studies of local shrine communities, analysis of local religious texts, and examination of resistance movements or adaptations that preserved distinct Shinto elements.',
    'Greater demonstrated agency would suggest a more genuinely coordinated (Rope-like) aspect, reducing the effective extraction from these groups. Less agency would confirm their position as targets of extraction.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(historical_agency_of_kami_worshippers, empirical, 'The degree of agency retained by Kami worshippers within the syncretic framework.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(shinbutsu_coexistence_commitment__syncretic_fusion_reading, 0, 100).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(shin_tr_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 0, 0.3).
narrative_ontology:measurement(shin_tr_t20, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 20, 0.33).
narrative_ontology:measurement(shin_tr_t40, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 40, 0.36).
narrative_ontology:measurement(shin_tr_t60, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 60, 0.38).
narrative_ontology:measurement(shin_tr_t80, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 80, 0.4).
narrative_ontology:measurement(shin_tr_t100, shinbutsu_coexistence_commitment__syncretic_fusion_reading, theater_ratio, 100, 0.4).

% Extraction over time
narrative_ontology:measurement(shin_be_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(shin_be_t20, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(shin_be_t40, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 40, 0.6).
narrative_ontology:measurement(shin_be_t60, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 60, 0.63).
narrative_ontology:measurement(shin_be_t80, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 80, 0.65).
narrative_ontology:measurement(shin_be_t100, shinbutsu_coexistence_commitment__syncretic_fusion_reading, base_extractiveness, 100, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(shin_su_t0, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(shin_su_t20, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 20, 0.65).
narrative_ontology:measurement(shin_su_t40, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 40, 0.7).
narrative_ontology:measurement(shin_su_t60, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 60, 0.73).
narrative_ontology:measurement(shin_su_t80, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 80, 0.75).
narrative_ontology:measurement(shin_su_t100, shinbutsu_coexistence_commitment__syncretic_fusion_reading, suppression_requirement, 100, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(shinbutsu_coexistence_commitment__syncretic_fusion_reading, identity_coordination).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'shinbutsu_coexistence_commitment' kernel, focusing on the ontological unification via honji suijaku. Sibling readings include 'domain_partition_reading' and 'incoherent_bundle_reading'.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
