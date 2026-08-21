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
    narrative_ontology:epsilon_provenance/5,
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
 *   This constraint models the 'living document' reading of Magna Carta,
 *   where its original meaning is legitimately superseded by an evolving
 *   interpretive tradition and precedential accumulation constitutes
 *   constitutional development. This reading views Magna Carta not as a fixed
 *   historical artifact, but as a foundational text whose principles adapt to
 *   contemporary societal needs through judicial and legislative
 *   interpretation. It is a meta-constraint on interpretive authority,
 *   allowing for flexibility and ongoing relevance.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__living_document_reading, 0.25).
domain_priors:suppression_score(magna_carta_1215__living_document_reading, 0.15).
domain_priors:theater_ratio(magna_carta_1215__living_document_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, suppression_requirement, 0.15).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(magna_carta_1215__living_document_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__living_document_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__living_document_reading, "Magna Carta as Living Constitutional Document").
narrative_ontology:topic_domain(magna_carta_1215__living_document_reading, "constitutional_law/legal_history/political_theory").

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__living_document_reading, 'af2ba917-9b2f-4b03-b7c2-399efb891045').
narrative_ontology:cs_kernel_codification('af2ba917-9b2f-4b03-b7c2-399efb891045', fixed_text).
narrative_ontology:cs_authority_grounding('af2ba917-9b2f-4b03-b7c2-399efb891045', lineage).
narrative_ontology:cs_interpretation_layer_present('af2ba917-9b2f-4b03-b7c2-399efb891045').
narrative_ontology:cs_reading_relation('af2ba917-9b2f-4b03-b7c2-399efb891045', magna_carta_1215__baronial_privilege_reading, coexists_with).
narrative_ontology:cs_reading_relation('af2ba917-9b2f-4b03-b7c2-399efb891045', magna_carta_1215__universal_rights_reading, coexists_with).
narrative_ontology:cs_axiom('af2ba917-9b2f-4b03-b7c2-399efb891045', foundational, constitutional_meaning_evolves).
narrative_ontology:cs_axiom_status(constitutional_meaning_evolves, holdable).
narrative_ontology:cs_axiom_grounding('af2ba917-9b2f-4b03-b7c2-399efb891045', constitutional_meaning_evolves, conventional).
narrative_ontology:cs_axiom('af2ba917-9b2f-4b03-b7c2-399efb891045', foundational, precedent_constitutes_development).
narrative_ontology:cs_axiom_status(precedent_constitutes_development, holdable).
narrative_ontology:cs_axiom_grounding('af2ba917-9b2f-4b03-b7c2-399efb891045', precedent_constitutes_development, conventional).
narrative_ontology:cs_reference_frame('af2ba917-9b2f-4b03-b7c2-399efb891045', evolving_constitutional_tradition).
narrative_ontology:cs_drift_state('af2ba917-9b2f-4b03-b7c2-399efb891045', contemporary, gap(stable, minor, true)).
narrative_ontology:cs_created_at('af2ba917-9b2f-4b03-b7c2-399efb891045', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__living_document_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, constitutional_scholars).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, judiciary).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, legislature).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(magna_carta_1215__living_document_reading, general_public).
narrative_ontology:constraint_victim(magna_carta_1215__living_document_reading, originalists).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, constitutional_evolution_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_1215__living_document_reading, judicial_review_legitimacy).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefit from the ongoing interpretive work, which provides a rich field for academic inquiry and allows for the adaptation of historical texts to contemporary issues. Their work contributes to the interpretive tradition.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, constitutional_scholars, beneficiary,
    analytical, generational, analytical, global).

% Act as primary interpreters, applying Magna Carta's principles through evolving case law. This reading grants them flexibility to adapt the document's meaning to new social contexts, enhancing their perceived legitimacy and power.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, judiciary, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the flexibility this reading provides, allowing for statutory interpretation and constitutional amendment that builds upon, rather than being strictly bound by, original intent. It enables legislative innovation within a perceived constitutional framework.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, legislature, beneficiary,
    institutional, generational, constrained, national).

% Bear the cost of their preferred interpretive method being superseded. They argue for strict adherence to original meaning, finding their interpretive framework marginalized by the 'living document' approach, which they see as judicial overreach.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, originalists, payer,
    organized, biographical, identity_locked, national).

% Benefits from a constitutional framework that can adapt to modern challenges and values, ensuring relevance and perceived fairness over time. However, they may also experience a sense of detachment from the 'original' document.
narrative_ontology:constraint_stakeholder(magna_carta_1215__living_document_reading, general_public, beneficiary,
    moderate, biographical, constrained, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for constitutional interpretation that allows for adaptation and evolution, coordinating legal development across generations without requiring constant formal amendment.
% TRANSFER_FUNCTION: Transfers interpretive authority from historical intent to contemporary legal and social consensus, enabling the judiciary and legislature to adapt constitutional principles.
% ABSENT_VOICES: Strict textualists and those who believe constitutional meaning should be fixed at its founding are present but marginalized in this interpretive tradition; they would argue for a return to original intent.
% DISAPPEARANCE_RATIONALE: If the 'living document' reading of Magna Carta vanished, the entire edifice of common law and constitutional interpretation would collapse, requiring a complete re-evaluation of legal precedent and the role of the judiciary and legislature in constitutional development. The legal system would revert to a strict originalist or textualist approach, fundamentally altering governance.
% FOUNDING_PROBLEM: The problem of how an ancient document, drafted for a specific feudal context, could remain relevant and authoritative in a continuously evolving society and legal system.
% FOUNDING_PROBLEM_CORROBORATION: Constitutional scholars and the judiciary widely attest to the ongoing challenge of adapting foundational texts to modern contexts, citing the need for a flexible interpretive framework to maintain constitutional legitimacy and prevent obsolescence. This is corroborated by the continuous evolution of legal precedent and societal norms.
narrative_ontology:disappearance_verdict(magna_carta_1215__living_document_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__living_document_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__living_document_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_1215__living_document_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__living_document_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low (0.25) because this reading primarily facilitates adaptation rather than imposing direct costs, though it does extract from those who prefer a fixed interpretation. Suppression is low (0.15) as it primarily operates through interpretive consensus rather than overt coercion. Theater ratio is low (0.1) because the interpretive work is genuine, even if it re-frames the original intent. The metrics reflect a dynamic, adaptive constraint that coordinates legal evolution.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the judiciary and legislature, this is a necessary and beneficial interpretive framework. From the perspective of originalists, it represents an illegitimate departure from foundational principles. The engine's classification will reflect this divergence based on their structural positions.
 *
 * DIRECTIONALITY LOGIC:
 *   Constitutional scholars, the judiciary, and the legislature are beneficiaries, gaining flexibility and interpretive power. Originalists are payers, as their preferred interpretive method is de-emphasized. The general public is a beneficiary of an adaptable constitution, though they may bear indirect costs of interpretive shifts.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    interpretive_legitimacy_source,
    'Does the legitimacy of constitutional interpretation derive from fidelity to original intent, or from its capacity to adapt to contemporary values and societal needs?',
    'Philosophical and legal debate, potentially resolved by shifts in judicial philosophy or public consensus over generations.',
    'If original intent is deemed the sole source of legitimacy, this ''living document'' reading would be reclassified as a Snare, as it would be seen as an illegitimate extraction of interpretive authority. If adaptation is paramount, its Rope classification is reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(interpretive_legitimacy_source, conceptual, 'Ambiguity regarding the ultimate source of constitutional interpretive legitimacy.').

omega_variable(
    interpretive_drift_accountability,
    'At what point does interpretive ''development'' become an unconstrained re-writing of the foundational text, and who holds the interpreters accountable?',
    'Empirical analysis of interpretive shifts against public opinion, legislative action, and formal amendment processes; comparative legal studies of constitutional amendment vs. judicial interpretation.',
    'If interpretive drift is found to be unchecked and unaccountable, the constraint''s extractiveness and suppression would be re-evaluated upwards, potentially shifting its classification towards a Tangled Rope or Snare, as it would represent an unaccountable power transfer.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_drift_accountability, empirical, 'The boundary between legitimate constitutional evolution and unaccountable interpretive overreach.').

omega_variable(
    kernel_reading_identity,
    'Is this constraint a genuine ''reading'' of the Magna Carta kernel, or does it constitute a distinct, new constraint that merely references the kernel for legitimacy?',
    'Analysis of the interpretive chain: does it maintain a plausible, if evolving, connection to the text, or does it functionally abandon the text for new principles? This is a conceptual distinction.',
    'If it''s deemed a new constraint, its classification would be re-evaluated independently of the Magna Carta kernel, potentially revealing higher extraction if its legitimacy is purely self-referential. If it''s a true reading, its legitimacy is partly inherited from the kernel.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(kernel_reading_identity, conceptual, 'Distinction between a ''reading'' and a new constraint leveraging historical legitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__living_document_reading, 1215, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__living_document_reading, theater_ratio, 1215, 0.01).
narrative_ontology:measurement(magn_tr_t1688, magna_carta_1215__living_document_reading, theater_ratio, 1688, 0.03).
narrative_ontology:measurement(magn_tr_t1787, magna_carta_1215__living_document_reading, theater_ratio, 1787, 0.05).
narrative_ontology:measurement(magn_tr_t1900, magna_carta_1215__living_document_reading, theater_ratio, 1900, 0.08).
narrative_ontology:measurement(magn_tr_t2024, magna_carta_1215__living_document_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__living_document_reading, base_extractiveness, 1215, 0.05).
narrative_ontology:measurement(magn_be_t1688, magna_carta_1215__living_document_reading, base_extractiveness, 1688, 0.1).
narrative_ontology:measurement(magn_be_t1787, magna_carta_1215__living_document_reading, base_extractiveness, 1787, 0.15).
narrative_ontology:measurement(magn_be_t1900, magna_carta_1215__living_document_reading, base_extractiveness, 1900, 0.2).
narrative_ontology:measurement(magn_be_t2024, magna_carta_1215__living_document_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__living_document_reading, suppression_requirement, 1215, 0.05).
narrative_ontology:measurement(magn_su_t1688, magna_carta_1215__living_document_reading, suppression_requirement, 1688, 0.08).
narrative_ontology:measurement(magn_su_t1787, magna_carta_1215__living_document_reading, suppression_requirement, 1787, 0.1).
narrative_ontology:measurement(magn_su_t1900, magna_carta_1215__living_document_reading, suppression_requirement, 1900, 0.12).
narrative_ontology:measurement(magn_su_t2024, magna_carta_1215__living_document_reading, suppression_requirement, 2024, 0.15).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__living_document_reading, identity_coordination).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__baronial_privilege_reading).
narrative_ontology:affects_constraint(magna_carta_1215__living_document_reading, magna_carta_1215__universal_rights_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Magna Carta (1215) kernel. This 'living document' reading emphasizes adaptive interpretation, while the 'baronial privilege' reading focuses on original feudal context and the 'universal rights' reading emphasizes transhistorical principles. All three are linked as a constraint family.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
