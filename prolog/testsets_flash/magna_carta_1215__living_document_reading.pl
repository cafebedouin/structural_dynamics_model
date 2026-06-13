% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__living_document_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__living_document_reading, []).

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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: magna_carta_1215__living_document_reading
 *   human_readable: Magna Carta as Living Constitutional Document
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint describes the 'living document' reading of Magna Carta,
 *   where its original meaning is legitimately superseded by an evolving
 *   interpretive tradition, and precedential accumulation constitutes
 *   constitutional development. It functions as a meta-constraint on
 *   interpretive authority, allowing the document to adapt over centuries.
 *   This reading is one of several competing interpretations of Magna Carta,
 *   each forming a distinct constraint.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, 0.2).
domain_priors:suppression_score(magna_carta_1215__living_document_reading, 0.1).
domain_priors:theater_ratio(magna_carta_1215__living_document_reading, 0.05).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, suppression_requirement, 0.1).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, theater_ratio, 0.05).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, resistance, 0.05).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__living_document_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__living_document_reading, "Magna Carta as Living Constitutional Document").
narrative_ontology:topic_domain(magna_carta_1215__living_document_reading, "constitutional_law/legal_history/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__living_document_reading, '11793385-fcdb-40cb-900e-768ddbac5e25').
narrative_ontology:cs_kernel_codification('11793385-fcdb-40cb-900e-768ddbac5e25', fixed_text).
narrative_ontology:cs_authority_grounding('11793385-fcdb-40cb-900e-768ddbac5e25', lineage).
narrative_ontology:cs_interpretation_layer_present('11793385-fcdb-40cb-900e-768ddbac5e25').
narrative_ontology:cs_reading_relation('11793385-fcdb-40cb-900e-768ddbac5e25', magna_carta_1215__baronial_privilege_reading, coexists_with).
narrative_ontology:cs_reading_relation('11793385-fcdb-40cb-900e-768ddbac5e25', magna_carta_1215__universal_rights_reading, coexists_with).
narrative_ontology:cs_axiom('11793385-fcdb-40cb-900e-768ddbac5e25', foundational, constitutional_meaning_evolves).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves, holdable).
narrative_ontology:cs_axiom_grounding('11793385-fcdb-40cb-900e-768ddbac5e25', constitutional_meaning_evolves, conventional).
narrative_ontology:cs_axiom('11793385-fcdb-40cb-900e-768ddbac5e25', foundational, precedent_constitutes_development).
narrative_ontology:cs_axiom_status(precedent_constitutes_development, holdable).
narrative_ontology:cs_axiom_grounding('11793385-fcdb-40cb-900e-768ddbac5e25', precedent_constitutes_development, conventional).
narrative_ontology:cs_reference_frame('11793385-fcdb-40cb-900e-768ddbac5e25', common_law_interpretive_tradition).
narrative_ontology:cs_drift_state('11793385-fcdb-40cb-900e-768ddbac5e25', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('11793385-fcdb-40cb-900e-768ddbac5e25', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__living_document_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, constitutional_scholars).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, legislature).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, citizens).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, originalists).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, constitutional_evolution_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, judicial_review_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets and applies Magna Carta's principles through case law, allowing its meaning to evolve. Benefits from the flexibility this reading provides for adapting law to modern contexts, but is constrained by the need to maintain continuity with past precedent.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Develop and propagate theories of constitutional interpretation that support the 'living document' concept. Their intellectual work provides the theoretical substrate for judicial and legislative adaptation, legitimizing their own field of study.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, constitutional_scholars, beneficiary,
    organized, generational, mobile, global).

% Benefits from the interpretive flexibility that allows new statutes to be reconciled with ancient constitutional principles, avoiding rigid adherence to original intent that might impede modern governance. Constrained by public and judicial expectations of constitutional fidelity.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, legislature, beneficiary,
    institutional, generational, constrained, national).

% Bear the cost of their interpretive framework being superseded. Their arguments for strict adherence to original meaning are marginalized or reinterpreted within this 'living document' framework, requiring constant re-articulation of their position against the dominant interpretive tradition.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, originalists, payer,
    organized, generational, identity_locked, national).

% Benefit from a constitutional framework that can adapt to contemporary social values and challenges, ensuring its continued relevance. Their ability to influence this evolution is indirect, primarily through political participation and advocacy.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, citizens, beneficiary,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for constitutional interpretation that allows for the adaptation of foundational legal principles to changing societal norms and challenges, ensuring the continued legitimacy and relevance of the constitution over centuries.
% TRANSFER_FUNCTION: Transfers interpretive authority from the original framers' intent to an ongoing process of judicial and legislative development, allowing for the re-allocation of rights and duties in response to evolving social conditions.
% ABSENT_VOICES: Future generations, whose interests are theoretically represented by the adaptive nature of the document, but who have no direct voice in its contemporary interpretation. Their 'voice' is channeled through the interpretive tradition itself.
% DISAPPEARANCE_RATIONALE: If the 'living document' reading vanished, the constitutional system would revert to a more rigid, originalist interpretation, leading to significant legal and political upheaval as many established precedents and statutes would be challenged as unconstitutional. The entire legal-historical narrative of constitutional development would collapse.
% FOUNDING_PROBLEM: The problem of how an ancient document, drafted for a specific feudal context, could remain relevant and authoritative in vastly different modern societies without being constantly rewritten or losing its foundational status.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and legal historians widely corroborate the ongoing challenge of maintaining constitutional relevance across centuries. The judiciary's consistent practice of reinterpreting foundational texts, and the legislature's reliance on this flexibility, further attest to the problem's live status, from outside the immediate beneficiaries of this specific reading.
narrative_ontology:disappearance_verdict(magna_carta_1215__living_document_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__living_document_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__living_document_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_1215__living_document_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__living_document_reading_tests).
:- end_tests(magna_carta_1215__living_document_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.2) is low, representing the 'cost' of maintaining an adaptive interpretive tradition, primarily borne by those whose rigid interpretations are superseded. Suppression (0.1) is also low, as this reading primarily operates through intellectual and judicial persuasion rather than overt coercion. Theater ratio (0.05) is minimal, reflecting that the interpretive work is genuinely functional in adapting the constitution. Accessibility collapse (0.7) is moderate-high, as once this interpretive framework is adopted, alternatives like strict originalism become less viable within the mainstream legal discourse. Resistance (0.05) is low, as this reading is widely accepted within the legal establishment, though contested by specific factions.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the judiciary and scholars, this reading is a necessary and beneficial adaptation (Rope-like). From the perspective of originalists, it represents an illegitimate departure from foundational principles, effectively extracting interpretive authority from the original text (Snare-like for them). The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The judiciary, constitutional scholars, and the legislature are beneficiaries (d near 0.0-0.2) as this reading grants them interpretive flexibility and legitimizes their roles in constitutional development. Originalists are payers (d near 0.8-1.0) as their interpretive framework is actively challenged and often overridden by this reading. Citizens are diffuse beneficiaries, gaining from a relevant and adaptable constitution.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_authority_legitimacy,
    'Does the interpretive tradition genuinely derive its legitimacy from the original document, or has it become an independent source of authority that merely uses Magna Carta as a symbolic anchor?',
    'Historical-legal analysis tracing the chain of interpretive authority and its relationship to formal amendment processes. Examination of whether the interpretive tradition could persist if the original document were disavowed.',
    'If it''s an independent authority, the constraint''s ''rope'' classification might shift towards ''tangled_rope'' or ''snare'' for those who believe the interpretive authority has become self-serving or detached from its source. If it genuinely derives from the document, the ''rope'' classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_legitimacy, conceptual, 'Ambiguity regarding the source of legitimacy for the interpretive tradition.').

omega_variable(
    scope_of_adaptation,
    'What are the inherent limits to constitutional adaptation through interpretation before it becomes an effective amendment without formal process?',
    'Comparative constitutional analysis of systems with different amendment thresholds and interpretive traditions. Legal-philosophical inquiry into the nature of ''interpretation'' versus ''creation'' in constitutional law.',
    'If adaptation is found to routinely exceed the bounds of interpretation, the constraint might be reclassified as a ''snare'' for those whose originalist expectations are consistently overridden, or a ''tangled_rope'' if the interpretive process itself becomes a site of unacknowledged extraction.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_adaptation, conceptual, 'The boundary between legitimate interpretation and de facto amendment.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__living_document_reading, 1215, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__living_document_reading, theater_ratio, 1215, 0.01).
narrative_ontology:measurement(magn_tr_t1600, magna_carta_1215__living_document_reading, theater_ratio, 1600, 0.02).
narrative_ontology:measurement(magn_tr_t1800, magna_carta_1215__living_document_reading, theater_ratio, 1800, 0.03).
narrative_ontology:measurement(magn_tr_t1900, magna_carta_1215__living_document_reading, theater_ratio, 1900, 0.04).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_1215__living_document_reading, theater_ratio, 2024, 0.05).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__living_document_reading, base_extractiveness, 1215, 0.1).
narrative_ontology:measurement(magn_be_t1600, magna_carta_1215__living_document_reading, base_extractiveness, 1600, 0.15).
narrative_ontology:measurement(magn_be_t1800, magna_carta_1215__living_document_reading, base_extractiveness, 1800, 0.18).
narrative_ontology:measurement(magn_be_t1900, magna_carta_1215__living_document_reading, base_extractiveness, 1900, 0.19).
narrative_ontology:measurement(magn_be_t2024, magna_carta_1215__living_document_reading, base_extractiveness, 2024, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__living_document_reading, suppression_requirement, 1215, 0.05).
narrative_ontology:measurement(magn_su_t1600, magna_carta_1215__living_document_reading, suppression_requirement, 1600, 0.08).
narrative_ontology:measurement(magn_su_t1800, magna_carta_1215__living_document_reading, suppression_requirement, 1800, 0.09).
narrative_ontology:measurement(magn_su_t1900, magna_carta_1215__living_document_reading, suppression_requirement, 1900, 0.1).
narrative_ontology:measurement(magn_su_t2024, magna_carta_1215__living_document_reading, suppression_requirement, 2024, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__living_document_reading, identity_coordination).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__universal_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of Magna Carta (1215), each with its own structural properties. This 'living document' reading provides the adaptive framework that influences how the other readings are understood and applied in contemporary legal contexts.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
