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
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: magna_carta_clause_39__feudal_prerogative_reading
 *   human_readable: Magna Carta Clause 39 (Feudal Prerogative Reading)
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint represents the 'feudal prerogative' reading of Magna
 *   Carta's Clause 39, which interprets the clause as a narrow procedural
 *   right granted to 'free men' (primarily barons and other elite landowners)
 *   within the existing hierarchical feudal order. It was designed to limit
 *   specific arbitrary actions by the Crown against its powerful subjects,
 *   not to establish universal rights. The constraint's primary function was
 *   to stabilize the relationship between the monarch and the nobility,
 *   preserving the overall structure of feudal authority while mitigating its
 *   most egregious abuses against the elite.
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
narrative_ontology:constraint_claim(magna_carta_clause_39__feudal_prerogative_reading, rope).
narrative_ontology:human_readable(magna_carta_clause_39__feudal_prerogative_reading, "Magna Carta Clause 39 (Feudal Prerogative Reading)").
narrative_ontology:topic_domain(magna_carta_clause_39__feudal_prerogative_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__feudal_prerogative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__feudal_prerogative_reading, '0d547bfb-a0a3-41a6-b34b-97e9a2330469').
narrative_ontology:cs_kernel_codification('0d547bfb-a0a3-41a6-b34b-97e9a2330469', fixed_text).
narrative_ontology:cs_authority_grounding('0d547bfb-a0a3-41a6-b34b-97e9a2330469', lineage).
narrative_ontology:cs_interpretation_layer_present('0d547bfb-a0a3-41a6-b34b-97e9a2330469').
narrative_ontology:cs_reading_relation('0d547bfb-a0a3-41a6-b34b-97e9a2330469', magna_carta_clause_39__liberal_due_process_reading, coexists_with).
narrative_ontology:cs_reading_relation('0d547bfb-a0a3-41a6-b34b-97e9a2330469', magna_carta_clause_39__originalist_limitation_reading, coexists_with).
narrative_ontology:cs_axiom('0d547bfb-a0a3-41a6-b34b-97e9a2330469', foundational, rights_are_feudal_grants).
narrative_ontology:cs_axiom_status(rights_are_feudal_grants, holdable).
narrative_ontology:cs_axiom_grounding('0d547bfb-a0a3-41a6-b34b-97e9a2330469', rights_are_feudal_grants, conventional).
narrative_ontology:cs_axiom('0d547bfb-a0a3-41a6-b34b-97e9a2330469', secondary, hierarchy_is_natural_order).
narrative_ontology:cs_axiom_status(hierarchy_is_natural_order, holdable).
narrative_ontology:cs_axiom_grounding('0d547bfb-a0a3-41a6-b34b-97e9a2330469', hierarchy_is_natural_order, theological).
narrative_ontology:cs_reference_frame('0d547bfb-a0a3-41a6-b34b-97e9a2330469', feudal_baronial_settlement).
narrative_ontology:cs_drift_state('0d547bfb-a0a3-41a6-b34b-97e9a2330469', contemporary_legal_discourse, gap(authority_erosion, severe, true)).
narrative_ontology:cs_created_at('0d547bfb-a0a3-41a6-b34b-97e9a2330469', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, barons_and_free_men).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, crown_authority).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, lower_classes).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__feudal_prerogative_reading, feudal_hierarchy_doctrine).
narrative_ontology:constraint_vindicates(magna_carta_clause_39__feudal_prerogative_reading, limited_monarchy_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The monarch and their direct agents, whose prerogative is affirmed and only narrowly constrained by Clause 39. They benefit from the stability of the feudal order and the limited nature of the rights asserted.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, crown_authority, agenda_setter,
    institutional, generational, constrained, national).

% The direct beneficiaries of the procedural rights granted by Clause 39, ensuring they are judged by their peers and according to established law, protecting their feudal holdings and status. Their power allows them to enforce these narrow rights.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, barons_and_free_men, beneficiary,
    powerful, biographical, mobile, national).

% Excluded from the protections of Clause 39, which applies only to 'free men' and their peers. They bear the cost of an unequal legal system that reinforces their subordinate position within the feudal hierarchy.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, lower_classes, payer,
    powerless, immediate, trapped, local).

% Analyze the historical context and original intent of Magna Carta, interpreting Clause 39 as a document primarily concerned with feudal rights and the balance of power among the elite, rather than universal human rights.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, legal_scholars_and_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the relationship between the Crown and its feudal tenants, establishing a baseline of procedural fairness for the elite to prevent arbitrary seizure of property or imprisonment, thereby stabilizing the feudal order.
% TRANSFER_FUNCTION: Transfers a limited guarantee of due process and judgment by peers to barons and free men, in exchange for their fealty and recognition of the Crown's overall authority. It implicitly transfers the burden of arbitrary justice to those outside the 'free man' class.
% ABSENT_VOICES: The vast majority of the population (serfs, unfree peasants) are absent from the conversation and are explicitly excluded from the protections of Clause 39. They would object to a system that formalizes their lack of legal recourse.
% DISAPPEARANCE_RATIONALE: If Clause 39 (as understood by this reading) vanished, the specific procedural guarantees for the feudal elite would disappear, potentially leading to increased arbitrary actions by the Crown against its powerful subjects, destabilizing the feudal power balance. However, the broader feudal hierarchy would likely persist, albeit with more friction at the top.
% FOUNDING_PROBLEM: The problem of arbitrary royal power infringing upon the established feudal rights and customs of the barons, leading to instability and rebellion.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and political theorists outside the direct beneficiaries corroborate that the specific feudal problems of 1215 are long dead, though the text's symbolic power persists. The Crown's authority is no longer primarily feudal, and the class of 'free men' with specific feudal rights no longer exists.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__feudal_prerogative_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__feudal_prerogative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__feudal_prerogative_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_clause_39__feudal_prerogative_reading, 'none', 1).

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
 *   Extractiveness is low (0.2) because this reading sees Clause 39 as a mutual agreement among elites, not a broad extractive mechanism. It primarily coordinates power within the existing hierarchy. Suppression is high (0.7) because the feudal system itself relied on significant coercion to maintain the social order, and Clause 39 did nothing to alleviate this for the majority. Theater ratio is low (0.1) as the clause had genuine, albeit limited, functional impact for its intended beneficiaries. The slight increase in theater over time reflects the gradual obsolescence of the specific feudal context.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the barons, Clause 39 was a vital 'rope' for coordinating power and limiting royal overreach. From the perspective of the lower classes, it was part of a 'snare' that formalized their exclusion from justice. The engine's classification will reflect this divergence based on the declared roles and exit options.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown and the barons are both beneficiaries, as the clause stabilizes their relationship and preserves their respective (though unequal) positions. The lower classes are victims, as the clause explicitly excludes them from its protections, reinforcing their subordinate status. Legal scholars and historians are observers, analyzing the historical context without direct participation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_free_men,
    'How broadly was ''free men'' interpreted in 13th-century England, and did this interpretation expand over time to include more social strata?',
    'Detailed historical analysis of court records and legal commentaries from the period, tracing the application of Clause 39 to different social groups.',
    'If ''free men'' was interpreted more broadly than initially assumed, the constraint''s victim set would shrink, and its coordination function would extend to a wider population, potentially shifting its classification towards a broader ''rope'' or even ''scaffold'' if the expansion was transitional.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_free_men, empirical, 'Ambiguity in the historical scope of beneficiaries.').

omega_variable(
    feudal_vs_universal_rights,
    'Is Clause 39 fundamentally a document of feudal prerogative, or does it contain latent principles of universal rights that transcend its historical context?',
    'Conceptual analysis of legal philosophy and historical jurisprudence, examining whether the principles of ''judgment by peers'' and ''law of the land'' inherently carry universal implications, regardless of the original intent.',
    'If universal principles are found to be latent, this reading would be seen as an incomplete or ''suppressed'' interpretation, and the constraint would be reclassified as a ''tangled_rope'' or ''snare'' from the perspective of those whose universal rights were denied.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(feudal_vs_universal_rights, conceptual, 'Conceptual debate over the inherent nature of the rights granted.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_clause_39__feudal_prerogative_reading, 1215, 1485).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1300, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1300, 0.12).
narrative_ontology:measurement(magn_tr_t1400, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1400, 0.15).
narrative_ontology:measurement(magn_tr_t1485, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1485, 0.18).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1215, 0.2).
narrative_ontology:measurement(magn_be_t1300, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1300, 0.18).
narrative_ontology:measurement(magn_be_t1400, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1400, 0.15).
narrative_ontology:measurement(magn_be_t1485, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1485, 0.12).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1215, 0.7).
narrative_ontology:measurement(magn_su_t1300, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1300, 0.65).
narrative_ontology:measurement(magn_su_t1400, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1400, 0.6).
narrative_ontology:measurement(magn_su_t1485, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1485, 0.55).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__feudal_prerogative_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three distinct readings of Magna Carta's Clause 39. This 'feudal prerogative' reading emphasizes its role in stabilizing the feudal order, distinct from readings that emphasize universal rights or narrow historical limitations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
