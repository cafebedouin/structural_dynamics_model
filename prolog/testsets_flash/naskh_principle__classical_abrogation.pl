% ============================================================================
% CONSTRAINT STORY: naskh_principle__classical_abrogation
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_naskh_principle__classical_abrogation, []).

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
 *   constraint_id: naskh_principle__classical_abrogation
 *   human_readable: Classical Abrogation (Naskh) Principle in Quranic Jurisprudence
 *   domain: islamic_jurisprudence/quranic_hermeneutics/legal_theory
 *
 * SUMMARY:
 *   The principle of Naskh (abrogation) in classical Islamic jurisprudence
 *   posits that later revealed Quranic verses can supersede earlier ones on
 *   the same legal or theological topic, based on their chronological order.
 *   This reading, 'classical_abrogation', establishes a clear hierarchy of
 *   legal rulings, providing certainty for jurists but at the cost of
 *   interpretive flexibility and potential theological tension when verses
 *   appear contradictory. It is a foundational hermeneutical tool for many
 *   classical schools of thought.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(naskh_principle__classical_abrogation, 0.6).
domain_priors:suppression_score(naskh_principle__classical_abrogation, 0.7).
domain_priors:theater_ratio(naskh_principle__classical_abrogation, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, extractiveness, 0.6).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(naskh_principle__classical_abrogation, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(naskh_principle__classical_abrogation, tangled_rope).
narrative_ontology:human_readable(naskh_principle__classical_abrogation, "Classical Abrogation (Naskh) Principle in Quranic Jurisprudence").
narrative_ontology:topic_domain(naskh_principle__classical_abrogation, "islamic_jurisprudence/quranic_hermeneutics/legal_theory").

domain_priors:requires_active_enforcement(naskh_principle__classical_abrogation).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(naskh_principle__classical_abrogation, '4cc62c23-110a-480c-8bae-075bb6b63d6e').
narrative_ontology:cs_kernel_codification('4cc62c23-110a-480c-8bae-075bb6b63d6e', fixed_text).
narrative_ontology:cs_authority_grounding('4cc62c23-110a-480c-8bae-075bb6b63d6e', lineage).
narrative_ontology:cs_interpretation_layer_present('4cc62c23-110a-480c-8bae-075bb6b63d6e').
narrative_ontology:cs_reading_relation('4cc62c23-110a-480c-8bae-075bb6b63d6e', naskh_principle__contextual_harmonization, forecloses).
narrative_ontology:cs_reading_relation('4cc62c23-110a-480c-8bae-075bb6b63d6e', naskh_principle__progressive_restriction, forecloses).
narrative_ontology:cs_axiom('4cc62c23-110a-480c-8bae-075bb6b63d6e', foundational, chronological_supersession_is_divine_will).
narrative_ontology:cs_axiom_status(chronological_supersession_is_divine_will, holdable).
narrative_ontology:cs_axiom_grounding('4cc62c23-110a-480c-8bae-075bb6b63d6e', chronological_supersession_is_divine_will, theological).
narrative_ontology:cs_axiom('4cc62c23-110a-480c-8bae-075bb6b63d6e', foundational, abrogated_verses_lose_legal_force).
narrative_ontology:cs_axiom_status(abrogated_verses_lose_legal_force, holdable).
narrative_ontology:cs_axiom_grounding('4cc62c23-110a-480c-8bae-075bb6b63d6e', abrogated_verses_lose_legal_force, conventional).
narrative_ontology:cs_reference_frame('4cc62c23-110a-480c-8bae-075bb6b63d6e', early_islamic_legal_methodology).
narrative_ontology:cs_drift_state('4cc62c23-110a-480c-8bae-075bb6b63d6e', contemporary_hermeneutical_debates, gap(authority_erosion, substantial, false)).
narrative_ontology:cs_created_at('4cc62c23-110a-480c-8bae-075bb6b63d6e', '').
narrative_ontology:cs_kernel_id(naskh_principle__classical_abrogation, naskh_principle).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, classical_jurists).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, legal_scholars).
narrative_ontology:constraint_beneficiary(naskh_principle__classical_abrogation, state_legal_systems).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, interpretive_flexibility).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, theological_coherence_advocates).
narrative_ontology:constraint_victim(naskh_principle__classical_abrogation, lay_muslims).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(naskh_principle__classical_abrogation, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(naskh_principle__classical_abrogation, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(naskh_principle__classical_abrogation_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(naskh_principle__classical_abrogation, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(naskh_principle__classical_abrogation_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.6) stems from the power granted to jurists to declare certain verses 'abrogated' and thus legally inert, which can be seen as an interpretive 'tax' on the full textual corpus. Suppression (0.7) is high because this principle is actively enforced within traditional legal education and fatwa issuance, often suppressing alternative hermeneutical approaches. The theater ratio (0.2) is relatively low, as the principle is genuinely applied, though its justification may involve some performative defense against charges of textual contradiction. The historical measurements reflect the principle's establishment and subsequent periods of contestation and re-affirmation.
 *
 * PERSPECTIVAL GAP:
 *   Classical jurists and legal scholars experience this as a necessary coordination mechanism for legal certainty, simplifying the application of divine law. However, advocates for interpretive flexibility and lay Muslims seeking direct engagement with the Quran may experience it as an extractive and suppressive constraint, limiting their ability to derive meaning or reconcile apparent contradictions without external mediation.
 *
 * DIRECTIONALITY LOGIC:
 *   Classical jurists and legal scholars are beneficiaries (d=0.0-0.2) as the principle provides them with a clear methodology and authority in legal derivation. State legal systems also benefit (d=0.1-0.3) from the legal certainty it provides. Interpretive flexibility and theological coherence advocates are victims (d=0.7-0.9) as their preferred modes of engagement are suppressed. Lay Muslims are also victims (d=0.6-0.8) as their direct access to the full Quranic text's legal implications is mediated and potentially restricted.
 *
 * MANDATROPHY ANALYSIS:
 *   The constraint's mandate is to resolve apparent contradictions in the Quran and provide legal clarity. While the problem of textual complexity remains 'live', the 'classical_abrogation' reading's status is 'contested' by alternative hermeneutics. The classification as a Tangled Rope reflects that it genuinely solves a coordination problem (legal certainty) but does so with significant, asymmetric extraction (loss of interpretive flexibility) and requires active enforcement to maintain its dominance against competing readings. If the founding problem were 'dead' (i.e., no textual contradictions were perceived), and the constraint persisted, it would lean towards a Piton or Snare.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    naskh_principle_kernel_reading,
    'Is this constraint a genuine divine principle or a human interpretive construct to manage textual complexity?',
    'Theological consensus across diverse schools of thought, or a re-evaluation of early Islamic sources for explicit divine mandate for chronological abrogation.',
    'If a human construct, its authority shifts from divine law to a conventional legal methodology, potentially reducing its suppressive force and extractiveness for those seeking alternative interpretations. This reading (classical_abrogation) would be reclassified as a conventional legal tool rather than a theological truth.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(naskh_principle_kernel_reading, conceptual, 'This constraint is the ''classical_abrogation'' reading of the ''naskh_principle'' kernel. Sibling readings (''contextual_harmonization'', ''progressive_restriction'') would challenge its theological grounding and legal finality.').

omega_variable(
    interpretive_flexibility_cost,
    'What is the true cost of reduced interpretive flexibility for theological coherence and modern legal application?',
    'Empirical study of legal disputes and theological debates in contexts where classical abrogation is strictly applied versus contexts where alternative hermeneutics are dominant.',
    'If the cost is high (e.g., leading to legal rigidity or perceived theological contradictions), it would increase the measured extractiveness and suppression of this reading, potentially pushing it closer to a Snare for those seeking more nuanced interpretations.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_flexibility_cost, empirical, 'The classical abrogation principle sacrifices interpretive flexibility for legal certainty, potentially creating theological tensions.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(naskh_principle__classical_abrogation, 0, 500).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(nask_tr_t0, naskh_principle__classical_abrogation, theater_ratio, 0, 0.1).
narrative_ontology:measurement(nask_tr_t100, naskh_principle__classical_abrogation, theater_ratio, 100, 0.15).
narrative_ontology:measurement(nask_tr_t200, naskh_principle__classical_abrogation, theater_ratio, 200, 0.2).
narrative_ontology:measurement(nask_tr_t300, naskh_principle__classical_abrogation, theater_ratio, 300, 0.2).
narrative_ontology:measurement(nask_tr_t400, naskh_principle__classical_abrogation, theater_ratio, 400, 0.18).
narrative_ontology:measurement(nask_tr_t500, naskh_principle__classical_abrogation, theater_ratio, 500, 0.15).

% Extraction over time
narrative_ontology:measurement(nask_be_t0, naskh_principle__classical_abrogation, base_extractiveness, 0, 0.5).
narrative_ontology:measurement(nask_be_t100, naskh_principle__classical_abrogation, base_extractiveness, 100, 0.55).
narrative_ontology:measurement(nask_be_t200, naskh_principle__classical_abrogation, base_extractiveness, 200, 0.6).
narrative_ontology:measurement(nask_be_t300, naskh_principle__classical_abrogation, base_extractiveness, 300, 0.6).
narrative_ontology:measurement(nask_be_t400, naskh_principle__classical_abrogation, base_extractiveness, 400, 0.58).
narrative_ontology:measurement(nask_be_t500, naskh_principle__classical_abrogation, base_extractiveness, 500, 0.55).

% Suppression requirement over time
narrative_ontology:measurement(nask_su_t0, naskh_principle__classical_abrogation, suppression_requirement, 0, 0.6).
narrative_ontology:measurement(nask_su_t100, naskh_principle__classical_abrogation, suppression_requirement, 100, 0.65).
narrative_ontology:measurement(nask_su_t200, naskh_principle__classical_abrogation, suppression_requirement, 200, 0.7).
narrative_ontology:measurement(nask_su_t300, naskh_principle__classical_abrogation, suppression_requirement, 300, 0.7).
narrative_ontology:measurement(nask_su_t400, naskh_principle__classical_abrogation, suppression_requirement, 400, 0.68).
narrative_ontology:measurement(nask_su_t500, naskh_principle__classical_abrogation, suppression_requirement, 500, 0.65).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(naskh_principle__classical_abrogation, enforcement_mechanism).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the 'naskh_principle' kernel. Other readings include 'contextual_harmonization' and 'progressive_restriction', which offer alternative methods for reconciling Quranic verses.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
