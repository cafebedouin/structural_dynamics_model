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
    narrative_ontology:stakeholder_secondary_role/3,
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
 *   constraint_id: magna_carta_clause_39__feudal_prerogative_reading
 *   human_readable: Magna Carta Clause 39: Feudal Prerogative Reading
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'feudal prerogative' reading of Magna
 *   Carta Clause 39, which interprets the clause as preserving narrow
 *   procedural rights for the feudal elite (barons and free men) within the
 *   established hierarchical order of 13th-century England. It is not seen as
 *   establishing universal individual rights, but rather as a specific
 *   agreement to limit royal abuses against a particular class, thereby
 *   stabilizing the feudal system. This reading emphasizes the historical
 *   context and the limited scope of the original document.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__feudal_prerogative_reading, 0.35).
domain_priors:suppression_score(magna_carta_clause_39__feudal_prerogative_reading, 0.45).
domain_priors:theater_ratio(magna_carta_clause_39__feudal_prerogative_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, extractiveness, 0.35).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0.45).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__feudal_prerogative_reading, rope).
narrative_ontology:human_readable(magna_carta_clause_39__feudal_prerogative_reading, "Magna Carta Clause 39: Feudal Prerogative Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__feudal_prerogative_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__feudal_prerogative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__feudal_prerogative_reading, '97c87c8e-dbec-4b48-ace4-443d4953ce03').
narrative_ontology:cs_kernel_codification('97c87c8e-dbec-4b48-ace4-443d4953ce03', fixed_text).
narrative_ontology:cs_authority_grounding('97c87c8e-dbec-4b48-ace4-443d4953ce03', lineage).
narrative_ontology:cs_interpretation_layer_present('97c87c8e-dbec-4b48-ace4-443d4953ce03').
narrative_ontology:cs_reading_relation('97c87c8e-dbec-4b48-ace4-443d4953ce03', magna_carta_clause_39__liberal_due_process_reading, forecloses).
narrative_ontology:cs_reading_relation('97c87c8e-dbec-4b48-ace4-443d4953ce03', magna_carta_clause_39__originalist_limitation_reading, coexists_with).
narrative_ontology:cs_axiom('97c87c8e-dbec-4b48-ace4-443d4953ce03', foundational, feudal_hierarchy_is_just).
narrative_ontology:cs_axiom_status(feudal_hierarchy_is_just, holdable).
narrative_ontology:cs_axiom_grounding('97c87c8e-dbec-4b48-ace4-443d4953ce03', feudal_hierarchy_is_just, deontological).
narrative_ontology:cs_axiom('97c87c8e-dbec-4b48-ace4-443d4953ce03', foundational, rights_are_privileges_of_status).
narrative_ontology:cs_axiom_status(rights_are_privileges_of_status, holdable).
narrative_ontology:cs_axiom_grounding('97c87c8e-dbec-4b48-ace4-443d4953ce03', rights_are_privileges_of_status, conventional).
narrative_ontology:cs_reference_frame('97c87c8e-dbec-4b48-ace4-443d4953ce03', feudal_customary_law).
narrative_ontology:cs_drift_state('97c87c8e-dbec-4b48-ace4-443d4953ce03', late_feudal_era, gap(stable, minor, true)).
narrative_ontology:cs_created_at('97c87c8e-dbec-4b48-ace4-443d4953ce03', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, barons_and_free_men).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, king_john).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As monarch, he held ultimate authority but was constrained by feudal custom and the power of the barons. Clause 39 extracted some of his arbitrary power by requiring due process for the elite, forcing him to adhere to established legal norms.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, king_john, agenda_setter,
    institutional, biographical, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__feudal_prerogative_reading, king_john, payer).

% The primary beneficiaries of Clause 39, gaining specific procedural rights against arbitrary royal action. They actively enforced the clause through collective action and feudal courts, preserving their status and privileges within the hierarchical order.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, barons_and_free_men, beneficiary,
    organized, generational, constrained, national).
narrative_ontology:stakeholder_secondary_role(magna_carta_clause_39__feudal_prerogative_reading, barons_and_free_men, agenda_setter).

% Not covered by the narrow procedural rights of Clause 39. Their legal status and protections were determined by local lords and custom, not by this specific royal charter. They were structurally outside the scope of this constraint.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, commoners_and_serfs, excluded,
    powerless, immediate, trapped, local).

% The institutions responsible for interpreting and applying feudal law, including the provisions of Magna Carta. They ensured the procedural rights for the elite were upheld, reinforcing the established legal order.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, feudal_courts, agenda_setter,
    institutional, generational, constrained, regional).

% Analyze the historical context and original intent of Clause 39, interpreting it as a document primarily concerned with feudal rights and the balance of power between the King and his elite vassals, rather than universal principles.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, legal_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__feudal_prerogative_reading, barons_and_free_men).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__feudal_prerogative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of royal power with the traditional rights and privileges of the feudal elite, preventing arbitrary actions that could destabilize the realm and ensuring a predictable legal framework for the nobility.
% TRANSFER_FUNCTION: Transfers some degree of arbitrary judicial and executive power from the King to the procedural rights of the barons and free men, ensuring they could not be dispossessed or imprisoned without due process according to feudal law.
% ABSENT_VOICES: Commoners and serfs, who had no standing under this clause and whose rights were not addressed. They would have advocated for broader protections beyond the elite, but their voices were not part of the 1215 negotiation.
% DISAPPEARANCE_RATIONALE: If Clause 39 and its enforcement vanished, the feudal order would have been more unstable, royal power more absolute for the elite, and the balance of power between the King and his vassals would have shifted dramatically, likely leading to increased conflict and arbitrary rule.
% FOUNDING_PROBLEM: King John's arbitrary rule, abuse of feudal custom, and excessive demands on his barons, which led to widespread discontent and open rebellion among the nobility.
% FOUNDING_PROBLEM_CORROBORATION: Historical accounts of the baronial rebellion, contemporary chronicles, and legal histories from outside the benefiting parties (e.g., later constitutional historians) corroborate the specific feudal abuses that led to the charter.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__feudal_prerogative_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__feudal_prerogative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__feudal_prerogative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(magna_carta_clause_39__feudal_prerogative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__feudal_prerogative_reading, 0.35, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is low because the clause primarily codified existing (or slightly adjusted) privileges for the elite, rather than imposing significant new burdens on them. It extracted some arbitrary power from the King, but within the bounds of feudal custom. Suppression is moderate, reflecting the inherent suppressive nature of the feudal system itself, but the clause *reduced* arbitrary suppression for the elite. Theater ratio is very low, as the procedural rights, though narrow, were genuinely exercised and enforced by the barons. Resistance to the clause itself from its beneficiaries was low, as it represented a gain for them.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the barons, Clause 39 was a necessary coordination mechanism to ensure stable governance and protect their traditional rights. From the King's perspective, it was an imposed limitation on his prerogative. From the perspective of commoners, the clause was largely irrelevant to their daily lives, highlighting the narrow scope of its protections.
 *
 * DIRECTIONALITY LOGIC:
 *   The barons and free men are clear beneficiaries, gaining specific procedural protections. King John is the victim, as his arbitrary power is curtailed. Commoners and serfs are excluded, as the clause does not apply to them. Feudal courts act as agenda-setters, interpreting and applying the clause within the legal framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_rights_ambiguity,
    'Is Clause 39''s language (''no free man'') strictly limited to the feudal elite, or does it contain latent universal principles that could be extended to all individuals?',
    'Analysis of later legal interpretations and historical developments that either explicitly extended or restricted the application of these rights beyond the original feudal context.',
    'If universal principles are found, the constraint''s victim set would expand dramatically, and its extractiveness against arbitrary power would be re-evaluated as much higher, potentially reclassifying it as a Snare or Tangled Rope from the perspective of state power.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_rights_ambiguity, conceptual, 'Ambiguity regarding the intended scope of ''free man'' in Clause 39.').

omega_variable(
    naturalness_of_feudal_order,
    'Is the feudal hierarchical order, which Clause 39 seeks to preserve, a ''natural'' or inevitable social structure, or a constructed one maintained by power and custom?',
    'Comparative historical analysis of different societal structures and the philosophical arguments for and against the ''naturalness'' of hierarchy.',
    'If the feudal order is seen as a constructed constraint, the ''feudal_prerogative_reading'' itself might be re-evaluated as a Tangled Rope or Snare, as it would be seen as coordinating extraction within a constructed, rather than natural, framework.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(naturalness_of_feudal_order, conceptual, 'Whether the feudal order is a natural or constructed constraint.').

omega_variable(
    effectiveness_of_baronial_enforcement,
    'How consistently and effectively were the procedural rights of Clause 39 enforced by the barons and feudal courts against royal overreach in the decades following 1215?',
    'Detailed historical case studies of legal disputes and royal actions, assessing the outcomes for barons claiming rights under Clause 39.',
    'If enforcement was weak or inconsistent, the constraint''s ''rope'' classification would be challenged, potentially shifting towards a ''piton'' (if rights atrophied) or a ''snare'' (if the clause was selectively ignored by the King while still extracting loyalty).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(effectiveness_of_baronial_enforcement, empirical, 'The actual historical efficacy of Clause 39''s enforcement.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__feudal_prerogative_reading, 1215, 1300).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1230, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1230, 0.1).
narrative_ontology:measurement(magn_tr_t1250, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1250, 0.1).
narrative_ontology:measurement(magn_tr_t1270, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1270, 0.1).
narrative_ontology:measurement(magn_tr_t1300, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1300, 0.1).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1215, 0.4).
narrative_ontology:measurement(magn_be_t1230, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1230, 0.38).
narrative_ontology:measurement(magn_be_t1250, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1250, 0.36).
narrative_ontology:measurement(magn_be_t1270, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1270, 0.35).
narrative_ontology:measurement(magn_be_t1300, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1300, 0.35).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1215, 0.5).
narrative_ontology:measurement(magn_su_t1230, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1230, 0.48).
narrative_ontology:measurement(magn_su_t1250, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1250, 0.46).
narrative_ontology:measurement(magn_su_t1270, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1270, 0.45).
narrative_ontology:measurement(magn_su_t1300, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1300, 0.45).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
