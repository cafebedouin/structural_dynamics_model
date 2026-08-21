% ============================================================================
% CONSTRAINT STORY: magna_carta_clause_39__feudal_prerogative_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_clause_39__feudal_prerogative_reading, []).

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
 *   constraint_id: magna_carta_clause_39__feudal_prerogative_reading
 *   human_readable: Magna Carta Clause 39: Feudal Prerogative Reading
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'feudal prerogative' reading of Magna
 *   Carta's Clause 39, which interprets the clause as a narrow procedural
 *   right protecting the traditional privileges of the English barons (free
 *   men) against arbitrary royal power, rather than a universal declaration
 *   of due process. It is a reading that emphasizes the hierarchical and
 *   contractual nature of feudal society. The constraint is classified as a
 *   Tangled Rope because it coordinates the relationship between the Crown
 *   and the barons while extracting from commoners through their exclusion
 *   from its protections, requiring active enforcement to maintain this
 *   specific interpretation.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__feudal_prerogative_reading, 0.2).
domain_priors:suppression_score(magna_carta_clause_39__feudal_prerogative_reading, 0.7).
domain_priors:theater_ratio(magna_carta_clause_39__feudal_prerogative_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, extractiveness, 0.2).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__feudal_prerogative_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__feudal_prerogative_reading, "Magna Carta Clause 39: Feudal Prerogative Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__feudal_prerogative_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__feudal_prerogative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__feudal_prerogative_reading, 'e87de376-c112-4103-86b8-abdd2065137a').
narrative_ontology:cs_kernel_codification('e87de376-c112-4103-86b8-abdd2065137a', fixed_text).
narrative_ontology:cs_authority_grounding('e87de376-c112-4103-86b8-abdd2065137a', lineage).
narrative_ontology:cs_interpretation_layer_present('e87de376-c112-4103-86b8-abdd2065137a').
narrative_ontology:cs_reading_relation('e87de376-c112-4103-86b8-abdd2065137a', magna_carta_clause_39__liberal_due_process_reading, coexists_with).
narrative_ontology:cs_reading_relation('e87de376-c112-4103-86b8-abdd2065137a', magna_carta_clause_39__originalist_limitation_reading, coexists_with).
narrative_ontology:cs_axiom('e87de376-c112-4103-86b8-abdd2065137a', foundational, feudal_contract_supremacy).
narrative_ontology:cs_axiom_status(feudal_contract_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('e87de376-c112-4103-86b8-abdd2065137a', feudal_contract_supremacy, conventional).
narrative_ontology:cs_axiom('e87de376-c112-4103-86b8-abdd2065137a', foundational, class_specific_rights).
narrative_ontology:cs_axiom_status(class_specific_rights, holdable).
narrative_ontology:cs_axiom_grounding('e87de376-c112-4103-86b8-abdd2065137a', class_specific_rights, conventional).
narrative_ontology:cs_reference_frame('e87de376-c112-4103-86b8-abdd2065137a', feudal_legal_order_1215).
narrative_ontology:cs_drift_state('e87de376-c112-4103-86b8-abdd2065137a', contemporary_legal_discourse, gap(authority_erosion, substantial, true)).
narrative_ontology:cs_created_at('e87de376-c112-4103-86b8-abdd2065137a', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, barons).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, crown).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, commoners).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary beneficiaries of Clause 39 under this reading, securing their traditional feudal rights and privileges against arbitrary royal action, ensuring they are judged by their peers and according to established law. Their power is derived from land ownership and military capacity.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, barons, beneficiary,
    powerful, generational, constrained, national).

% The Crown, while constrained by Clause 39, also benefits from the stability of the feudal order it helps to maintain. The constraint formalizes a reciprocal relationship, limiting royal power in specific ways but legitimizing its overall authority within the established hierarchy. The Crown enforces the clause when it serves its interests in maintaining order.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, crown, agenda_setter,
    institutional, generational, constrained, national).

% Commoners are largely excluded from the protections of Clause 39 under this reading, as its rights apply primarily to 'free men' (nobles and gentry). They bear the costs of a hierarchical system that offers them little recourse against arbitrary power, whether from the Crown or their feudal lords. Their options are limited to local custom or direct petition, with no systemic legal protection.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, commoners, payer,
    powerless, immediate, trapped, local).

% Academically analyze the historical context and original intent of Magna Carta, interpreting Clause 39 as a document primarily concerned with feudal rights rather than universal liberties. Their work informs the 'feudal prerogative' reading.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the relationship between the Crown and its powerful vassals (barons) by formalizing traditional feudal rights and limiting arbitrary royal actions against them, thereby stabilizing the hierarchical order.
% TRANSFER_FUNCTION: Transfers the right to arbitrary judgment from the Crown to a system of judgment by peers and established law for 'free men' (primarily nobles), while implicitly affirming the Crown's authority over commoners.
% ABSENT_VOICES: Commoners, who would argue for broader protections against arbitrary power for all subjects, are absent from the negotiation and the direct benefits of this clause. Their voices are suppressed by the feudal structure itself.
% DISAPPEARANCE_RATIONALE: If Clause 39, as understood through this reading, vanished, the delicate balance of power between the Crown and the barons would be destabilized, potentially leading to increased conflict or a shift towards more absolute monarchy. The feudal legal order would lose a key stabilizing element.
% FOUNDING_PROBLEM: The problem of arbitrary royal power infringing upon the traditional rights and privileges of the English barons, leading to instability and rebellion.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and political theorists corroborate that the specific problem of feudal baronial rights against a medieval monarch is long dead. However, the Crown and some traditionalist legal scholars might argue that the principle of limiting executive overreach, even if for a narrow class, remains live.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__feudal_prerogative_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__feudal_prerogative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__feudal_prerogative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_clause_39__feudal_prerogative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__feudal_prerogative_reading, 0.2, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).
:- end_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is low (0.2) because, under this reading, the clause primarily formalizes existing feudal arrangements rather than creating new, highly extractive mechanisms. It extracts from commoners by denying them the same protections, but this is a feature of the background feudal system, not the clause's primary function. Suppression is high (0.7) because the feudal order itself, which this reading reinforces, relies on active suppression of commoner agency and alternatives. Theater ratio is low (0.1) as the clause genuinely served its intended function of coordinating elite power dynamics within the feudal system during this period. Accessibility collapse is high (0.8) for commoners, as the legal system offers them no alternative to their subordinate status under this interpretation. Resistance is low (0.15) because commoners lacked the organized power to resist this specific interpretation, though broader peasant revolts occurred for other reasons.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the barons, this reading of Clause 39 is a legitimate coordination mechanism that protects their ancient liberties. From the perspective of commoners, it is part of a broader system of extraction and suppression that offers them no recourse. The engine's classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The barons are clear beneficiaries, securing their rights. The Crown is an agenda-setter and also a beneficiary, as the clause stabilizes its relationship with powerful vassals. Commoners are victims, excluded from protection. Legal historians are observers, analyzing the historical context.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling a historical, class-specific coordination as a universal due process right. By focusing on the specific beneficiaries (barons) and victims (commoners) of the feudal era, it highlights how the constraint's function was tied to a particular social order. The founding problem (arbitrary royal power against barons) is 'dead' in its original form, but the constraint persisted due to its reinterpretation in later eras, which is addressed by other readings of this kernel. This reading itself, however, accurately reflects a constraint whose mandate was fulfilled within its historical context.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_free_men,
    'How broadly was ''free men'' interpreted in 13th-17th century English law, and did it include any commoners with property or specific legal standing?',
    'Detailed historical legal scholarship examining court records and legal treatises from the period to identify who was actually afforded the protections of Clause 39.',
    'If ''free men'' was interpreted more broadly than just the nobility, the victim set (commoners) would be smaller, and the extractiveness from commoners would be slightly lower, potentially shifting the classification closer to a pure Rope for a larger segment of the population.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_free_men, empirical, 'Ambiguity in the historical interpretation of ''free men'' in Clause 39.').

omega_variable(
    feudal_vs_universal_framing,
    'Is Clause 39 fundamentally a document of feudal contract law, or does it contain nascent principles of universal individual rights that transcend its immediate context?',
    'Conceptual analysis of legal philosophy and historical context, comparing the ''feudal prerogative'' reading with later ''liberal due process'' interpretations. This is a conceptual, not empirical, resolution.',
    'If framed as containing nascent universal rights, the constraint''s extractiveness from commoners would be seen as higher (due to denial of those rights), and its classification would shift towards a Snare or Tangled Rope, even in its early period, reflecting a missed opportunity for broader coordination.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feudal_vs_universal_framing, conceptual, 'Conceptual ambiguity regarding the fundamental nature of Clause 39 (feudal contract vs. universal rights).').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__feudal_prerogative_reading, 1215, 1688).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1350, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1350, 0.1).
narrative_ontology:measurement(magn_tr_t1485, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1485, 0.1).
narrative_ontology:measurement(magn_tr_t1600, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1600, 0.1).
narrative_ontology:measurement(magn_tr_t1688, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1688, 0.1).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1215, 0.2).
narrative_ontology:measurement(magn_be_t1350, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1350, 0.2).
narrative_ontology:measurement(magn_be_t1485, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1485, 0.2).
narrative_ontology:measurement(magn_be_t1600, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1600, 0.2).
narrative_ontology:measurement(magn_be_t1688, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1688, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1215, 0.7).
narrative_ontology:measurement(magn_su_t1350, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1350, 0.7).
narrative_ontology:measurement(magn_su_t1485, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1485, 0.7).
narrative_ontology:measurement(magn_su_t1600, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1600, 0.7).
narrative_ontology:measurement(magn_su_t1688, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1688, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
