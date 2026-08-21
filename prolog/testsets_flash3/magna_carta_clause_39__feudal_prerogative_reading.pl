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
 *   constraint_id: magna_carta_clause_39__feudal_prerogative_reading
 *   human_readable: Magna Carta Clause 39: Feudal Prerogative Reading
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint represents a 'feudal prerogative' reading of Magna
 *   Carta's Clause 39, which interprets the clause as a narrow procedural
 *   right protecting the traditional privileges of barons and free men within
 *   the existing hierarchical order, rather than establishing universal
 *   individual rights. It is a reading of the historical document that
 *   emphasizes its original, limited scope and its function in stabilizing
 *   feudal relations. The constraint is claimed as a Rope because it
 *   genuinely coordinated the relationship between the Crown and the
 *   nobility, providing mutual benefit within its specific historical
 *   context, despite its high suppression of those outside the feudal elite.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__feudal_prerogative_reading, 0.25).
domain_priors:suppression_score(magna_carta_clause_39__feudal_prerogative_reading, 0.7).
domain_priors:theater_ratio(magna_carta_clause_39__feudal_prerogative_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, resistance, 0.15).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__feudal_prerogative_reading, rope).
narrative_ontology:human_readable(magna_carta_clause_39__feudal_prerogative_reading, "Magna Carta Clause 39: Feudal Prerogative Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__feudal_prerogative_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__feudal_prerogative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__feudal_prerogative_reading, '9d5f6c60-be30-4c2c-8a54-f67bad16d781').
narrative_ontology:cs_kernel_codification('9d5f6c60-be30-4c2c-8a54-f67bad16d781', fixed_text).
narrative_ontology:cs_authority_grounding('9d5f6c60-be30-4c2c-8a54-f67bad16d781', lineage).
narrative_ontology:cs_interpretation_layer_present('9d5f6c60-be30-4c2c-8a54-f67bad16d781').
narrative_ontology:cs_reading_relation('9d5f6c60-be30-4c2c-8a54-f67bad16d781', magna_carta_clause_39__liberal_due_process_reading, forecloses).
narrative_ontology:cs_reading_relation('9d5f6c60-be30-4c2c-8a54-f67bad16d781', magna_carta_clause_39__originalist_limitation_reading, coexists_with).
narrative_ontology:cs_axiom('9d5f6c60-be30-4c2c-8a54-f67bad16d781', foundational, rights_are_feudal_privileges).
narrative_ontology:cs_axiom_status(rights_are_feudal_privileges, holdable).
narrative_ontology:cs_axiom_grounding('9d5f6c60-be30-4c2c-8a54-f67bad16d781', rights_are_feudal_privileges, conventional).
narrative_ontology:cs_axiom('9d5f6c60-be30-4c2c-8a54-f67bad16d781', secondary, crown_prerogative_is_supreme).
narrative_ontology:cs_axiom_status(crown_prerogative_is_supreme, holdable).
narrative_ontology:cs_axiom_grounding('9d5f6c60-be30-4c2c-8a54-f67bad16d781', crown_prerogative_is_supreme, conventional).
narrative_ontology:cs_reference_frame('9d5f6c60-be30-4c2c-8a54-f67bad16d781', feudal_baronial_order).
narrative_ontology:cs_drift_state('9d5f6c60-be30-4c2c-8a54-f67bad16d781', contemporary_legal_discourse, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('9d5f6c60-be30-4c2c-8a54-f67bad16d781', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, barons_and_free_men).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, crown_authority).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__feudal_prerogative_reading, feudal_hierarchy_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__feudal_prerogative_reading, crown_prerogative_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary beneficiaries of Clause 39 under this reading, securing their traditional rights and privileges against arbitrary royal action, but only within the established feudal order. Their 'peerage' status is key to their protection.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, barons_and_free_men, beneficiary,
    powerful, generational, constrained, national).

% The Crown, whose authority is constrained by Clause 39 but only in specific, traditional ways that do not fundamentally alter its prerogative. It benefits from the stability of the feudal order that the clause helps to maintain.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, crown_authority, agenda_setter,
    institutional, generational, constrained, national).

% Those outside the class of 'free men' who gain no direct protection from Clause 39 under this reading. Their rights are not addressed, and their status within the feudal hierarchy remains unchanged.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, unfree_peasants, excluded,
    powerless, immediate, trapped, local).

% Scholars who analyze the historical context and original intent of Magna Carta, often emphasizing its feudal origins and limited scope, providing the basis for this specific reading.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the relationship between the Crown and its feudal tenants, ensuring traditional rights are respected and preventing arbitrary royal actions that could destabilize the feudal hierarchy.
% TRANSFER_FUNCTION: Transfers a degree of procedural certainty and protection from arbitrary seizure or judgment to the feudal elite (barons and free men) from the Crown, in exchange for their continued loyalty and service within the established order.
% ABSENT_VOICES: Unfree peasants and other commoners are absent from the conversation; their interests and rights are not represented by Clause 39 under this reading, which focuses exclusively on the rights of the feudal elite.
% DISAPPEARANCE_RATIONALE: If Clause 39 (as understood by this reading) vanished, the delicate balance of power between the Crown and the barons would be destabilized, potentially leading to increased arbitrary royal actions and renewed feudal conflict, forcing a renegotiation of power.
% FOUNDING_PROBLEM: The problem of arbitrary royal power infringing upon the established feudal rights and customs of the barons, leading to instability and rebellion.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and political theorists outside the direct beneficiaries corroborate that the specific feudal problems of 1215 are no longer live, though the text's legacy persists. The Crown's modern successors do not claim the same prerogatives, and the feudal system itself is defunct.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__feudal_prerogative_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__feudal_prerogative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__feudal_prerogative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_clause_39__feudal_prerogative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__feudal_prerogative_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low because the clause primarily codified existing feudal rights rather than creating new burdens on the Crown, and it benefited the powerful barons. Suppression is high because the feudal system itself was highly coercive, and the clause did nothing to alleviate the lack of rights for the unfree. Theater ratio is low as the clause had genuine, immediate functional impact in its historical context. Accessibility collapse is high for those outside the feudal elite, as the clause did not open new avenues for justice for them. Resistance is low from the direct beneficiaries, as it served their interests.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the feudal elite, this was a beneficial coordination mechanism. From the perspective of the unfree, it was irrelevant or even reinforced their exclusion. The engine's classification will reflect the overall structural properties, but the per-seat classification for the unfree would be 'trapped' or 'snare' due to their lack of exit and high suppression.
 *
 * DIRECTIONALITY LOGIC:
 *   The barons and free men are clear beneficiaries, gaining protection for their established rights. The Crown, while constrained, also benefits from the stability of the feudal order and the continued loyalty of its powerful tenants. Unfree peasants are excluded, receiving no benefit and remaining subject to the existing suppressive structures. Legal historians act as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_free_men,
    'How broadly was ''free men'' interpreted in 13th-century England, and did this interpretation evolve to include a wider segment of the population?',
    'Detailed historical analysis of legal records, court proceedings, and contemporary commentaries on the application of Magna Carta in the decades following its promulgation.',
    'A broader historical interpretation of ''free men'' would increase the beneficiary set and potentially lower the effective suppression for a larger segment of the population, shifting the constraint closer to a broader ''Rope'' or even a nascent ''Scaffold'' for a wider public. A narrow interpretation reinforces its elite-focused nature.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_free_men, empirical, 'Ambiguity in the historical scope of ''free men'' protected by Clause 39.').

omega_variable(
    feudal_vs_universal_rights_framing,
    'Is Clause 39 fundamentally a document of feudal privilege, or does it contain latent principles that can be legitimately reinterpreted as foundational for universal individual rights?',
    'Conceptual analysis of legal philosophy and political theory, examining the logical coherence and historical justification of ''living constitution'' or ''evolving standards'' interpretations versus strict originalism.',
    'If reinterpreted as foundational for universal rights (the liberal due process reading), the constraint''s extractiveness would be seen as much lower, and its beneficiary set vastly expanded, shifting its classification towards a ''Rope'' or ''Mountain'' for all citizens. If strictly feudal, its scope remains narrow and its benefits limited to the elite.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feudal_vs_universal_rights_framing, conceptual, 'The conceptual framing of Clause 39 as either feudal privilege or universal rights.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__feudal_prerogative_reading, 1215, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1240, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1240, 0.09).
narrative_ontology:measurement(magn_tr_t1265, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1265, 0.08).
narrative_ontology:measurement(magn_tr_t1300, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1300, 0.07).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1215, 0.25).
narrative_ontology:measurement(magn_be_t1240, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1240, 0.23).
narrative_ontology:measurement(magn_be_t1265, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1265, 0.22).
narrative_ontology:measurement(magn_be_t1300, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1300, 0.2).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1215, 0.7).
narrative_ontology:measurement(magn_su_t1240, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1240, 0.68).
narrative_ontology:measurement(magn_su_t1265, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1265, 0.65).
narrative_ontology:measurement(magn_su_t1300, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1300, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__feudal_prerogative_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% This is one of three distinct readings of Magna Carta Clause 39. This 'feudal prerogative' reading emphasizes its original, limited scope and its function in stabilizing feudal relations, contrasting with later interpretations that see it as a foundation for universal rights or specific royal limitations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
