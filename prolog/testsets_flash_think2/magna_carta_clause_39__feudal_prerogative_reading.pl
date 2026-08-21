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
 *   constraint_id: magna_carta_clause_39__feudal_prerogative_reading
 *   human_readable: Magna Carta Clause 39: Feudal Prerogative Reading
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint story instantiates the 'feudal prerogative' reading of
 *   Magna Carta's Clause 39. In this reading, Clause 39 is understood as a
 *   narrow procedural protection for the English nobility against arbitrary
 *   royal power, operating strictly within the established hierarchical order
 *   of feudal society. It coordinates the relationship between the Crown and
 *   its powerful vassals, but implicitly reinforces the exclusion and
 *   extraction from commoners and unprivileged individuals, who are not
 *   afforded the same rights or standing.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_clause_39__feudal_prerogative_reading, 0.6).
domain_priors:suppression_score(magna_carta_clause_39__feudal_prerogative_reading, 0.7).
domain_priors:theater_ratio(magna_carta_clause_39__feudal_prerogative_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, extractiveness, 0.6).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 0.7).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, accessibility_collapse, 0.65).
narrative_ontology:constraint_metric(magna_carta_clause_39__feudal_prerogative_reading, resistance, 0.4).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_clause_39__feudal_prerogative_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_clause_39__feudal_prerogative_reading, "Magna Carta Clause 39: Feudal Prerogative Reading").
narrative_ontology:topic_domain(magna_carta_clause_39__feudal_prerogative_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_clause_39__feudal_prerogative_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_clause_39__feudal_prerogative_reading, 'e98fb3e0-5315-4d71-971f-0b015e23b219').
narrative_ontology:cs_kernel_codification('e98fb3e0-5315-4d71-971f-0b015e23b219', fixed_text).
narrative_ontology:cs_authority_grounding('e98fb3e0-5315-4d71-971f-0b015e23b219', lineage).
narrative_ontology:cs_interpretation_layer_present('e98fb3e0-5315-4d71-971f-0b015e23b219').
narrative_ontology:cs_reading_relation('e98fb3e0-5315-4d71-971f-0b015e23b219', magna_carta_clause_39__liberal_due_process_reading, forecloses).
narrative_ontology:cs_reading_relation('e98fb3e0-5315-4d71-971f-0b015e23b219', magna_carta_clause_39__originalist_limitation_reading, coexists_with).
narrative_ontology:cs_axiom('e98fb3e0-5315-4d71-971f-0b015e23b219', foundational, rights_are_hierarchical).
narrative_ontology:cs_axiom_status(rights_are_hierarchical, holdable).
narrative_ontology:cs_axiom_grounding('e98fb3e0-5315-4d71-971f-0b015e23b219', rights_are_hierarchical, deontological).
narrative_ontology:cs_axiom('e98fb3e0-5315-4d71-971f-0b015e23b219', foundational, crown_prerogative_limited_by_custom).
narrative_ontology:cs_axiom_status(crown_prerogative_limited_by_custom, holdable).
narrative_ontology:cs_axiom_grounding('e98fb3e0-5315-4d71-971f-0b015e23b219', crown_prerogative_limited_by_custom, conventional).
narrative_ontology:cs_reference_frame('e98fb3e0-5315-4d71-971f-0b015e23b219', feudal_legal_order_1215).
narrative_ontology:cs_drift_state('e98fb3e0-5315-4d71-971f-0b015e23b219', contemporary_legal_theory, gap(axiom_overriding, severe, true)).
narrative_ontology:cs_created_at('e98fb3e0-5315-4d71-971f-0b015e23b219', '').
narrative_ontology:cs_kernel_id(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, crown).
narrative_ontology:constraint_beneficiary(magna_carta_clause_39__feudal_prerogative_reading, feudal_lords).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, commoners).
narrative_ontology:constraint_victim(magna_carta_clause_39__feudal_prerogative_reading, unprivileged_individuals).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The monarch, whose power is nominally constrained by Clause 39 but whose overall authority and the feudal system it underpins are legitimized and stabilized by the agreement. Benefits from reduced baronial rebellion.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, crown, agenda_setter,
    institutional, generational, constrained, national).

% The barons and other high-ranking nobles who secured specific procedural rights ('lawful judgment of his peers or by the law of the land') against arbitrary royal power, thereby preserving their status and property within the feudal hierarchy.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, feudal_lords, beneficiary,
    powerful, generational, constrained, national).

% The vast majority of the population, who are explicitly excluded from the specific protections of Clause 39, remaining subject to the arbitrary will of their feudal lords and the Crown without recourse to 'judgment of peers' in the same sense.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, commoners, payer,
    powerless, immediate, trapped, local).

% Individuals of even lower status than commoners (e.g., serfs, villeins) whose rights are entirely subsumed by their feudal obligations, and for whom Clause 39 offers no protection whatsoever, implicitly reinforcing their subordinate position.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, unprivileged_individuals, payer,
    powerless, immediate, trapped, local).

% Contemporary legal interpreters who understood Clause 39 within the specific feudal context of 1215, recognizing its narrow scope and application primarily to the nobility, and documenting its role in stabilizing the existing order.
narrative_ontology:constraint_stakeholder(magna_carta_clause_39__feudal_prerogative_reading, legal_scholars_13th_century, observer,
    moderate, biographical, analytical, national).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_clause_39__feudal_prerogative_reading, crown).
narrative_ontology:fixing_cost_class(magna_carta_clause_39__feudal_prerogative_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates the exercise of royal power by establishing specific procedural limits on the Crown's actions against its feudal lords, thereby stabilizing the relationship between the monarch and the nobility and preventing widespread baronial rebellion.
% TRANSFER_FUNCTION: Transfers a degree of procedural security and status from the Crown to feudal lords, in exchange for their loyalty and military service, while implicitly transferring obedience and resources from commoners to the feudal system by maintaining their exclusion from these elite protections.
% ABSENT_VOICES: Commoners and unprivileged individuals are structurally absent from the negotiation and application of Clause 39; they would object to their exclusion from its protections and the reinforcement of their subordinate status within the feudal hierarchy.
% DISAPPEARANCE_RATIONALE: If Clause 39 and its underlying feudal prerogative vanished overnight in 1215, the delicate balance of power between the Crown and the barons would be destabilized, likely leading to increased arbitrary royal power or renewed civil strife among the elite. The feudal system would lose a key legitimizing document, potentially accelerating its transformation or collapse.
% FOUNDING_PROBLEM: Arbitrary royal power, particularly regarding the property, personal liberty, and customary rights of the nobility, which led to widespread baronial rebellion and threatened the stability of the kingdom.
% FOUNDING_PROBLEM_CORROBORATION: Historical accounts of the baronial rebellion of 1215, contemporary chronicles (e.g., Roger of Wendover, Matthew Paris), and later legal commentaries (e.g., Bracton) attest to the specific grievances of the nobility and the political context that necessitated the charter. These sources, from outside the direct beneficiaries, corroborate the problem's historical existence and its resolution within the feudal framework.
narrative_ontology:disappearance_verdict(magna_carta_clause_39__feudal_prerogative_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_clause_39__feudal_prerogative_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_clause_39__feudal_prerogative_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(magna_carta_clause_39__feudal_prerogative_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_clause_39__feudal_prerogative_reading, 0.6, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_clause_39__feudal_prerogative_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_clause_39__feudal_prerogative_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it genuinely coordinates the power dynamics between the Crown and the feudal lords (beneficiaries) by establishing procedural limits, but simultaneously enables asymmetric extraction from commoners and unprivileged individuals (victims) by explicitly *not* extending these protections to them, thereby legitimizing their subordinate status. Extractiveness is moderate-high (0.6-0.7) because the feudal system itself is highly extractive, and this clause helps maintain it. Suppression is high (0.7-0.8) as the feudal order relies on active enforcement of social hierarchy. Theater ratio is low (0.1) because the clause was a genuinely functional document for its time, addressing real grievances within the elite.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Crown and feudal lords, Clause 39 is a coordination mechanism that stabilizes the realm and protects their respective interests. From the perspective of commoners, it is part of the legal framework that legitimizes their exclusion and the extraction of their labor and resources. The engine's per-seat classification will reflect this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The Crown and feudal lords are beneficiaries, as the clause stabilizes their power and grants specific protections within their class. Commoners and unprivileged individuals are targets/victims, as the clause's narrow scope implicitly denies them similar protections and reinforces their subordinate position. The 13th-century legal scholars are observers, interpreting the clause within its historical context.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the constraint as a simple Rope (pure coordination) by highlighting the implicit extraction from the unprivileged. It also prevents mislabeling it as a Snare (pure extraction) by acknowledging the genuine coordination function among the elite. The founding problem (arbitrary royal power against nobility) is 'dead' in its original feudal context, but the constraint persists in legal lineage, leading to a 'contested' status for its contemporary relevance.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_rights_ambiguity,
    'Is Clause 39''s ''lawful judgment of his peers or by the law of the land'' truly restricted to feudal lords, or does it contain a latent universality that could be expanded?',
    'Analysis of subsequent legal developments (e.g., Statute of Westminster, Petition of Right) and judicial interpretations that expanded the scope of ''peers'' or ''law of the land'' beyond the nobility.',
    'If a latent universality is found, the constraint''s original extractiveness (from commoners) would be re-evaluated as a temporary, rather than inherent, feature, potentially shifting its historical classification towards a more nascent Rope for all subjects.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_rights_ambiguity, conceptual, 'Ambiguity regarding the intended and potential scope of Clause 39''s protections.').

omega_variable(
    implicit_extraction_quantification,
    'How much of the constraint''s persistence and stability within the feudal system relied on the implicit extraction from commoners whose rights were *not* protected by this clause?',
    'Historical economic analysis of feudal obligations and labor, combined with legal-historical studies of commoner grievances and lack of legal recourse in the period.',
    'A high quantification of implicit extraction would strengthen the Tangled Rope classification, emphasizing the coercive aspect of the coordination. A lower quantification might suggest a more balanced, albeit hierarchical, coordination.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(implicit_extraction_quantification, empirical, 'Quantifying the role of unacknowledged extraction in the constraint''s stability.').

omega_variable(
    naturalness_of_hierarchy,
    'Was the feudal hierarchy, which Clause 39 helped to stabilize, perceived as a natural and inevitable order, or a constructed one maintained by such legal constraints?',
    'Analysis of medieval political philosophy, theological doctrines on social order, and historical evidence of social mobility or peasant revolts challenging the ''naturalness'' of the hierarchy.',
    'If perceived as natural, the constraint''s suppression might be seen as less coercive. If constructed, the suppression and extractiveness would be viewed as more deliberate and less justified, potentially shifting the classification towards a Snare for the unprivileged.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(naturalness_of_hierarchy, conceptual, 'Whether the underlying social order was seen as natural or constructed.').


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
narrative_ontology:measurement(magn_tr_t1245, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1245, 0.1).
narrative_ontology:measurement(magn_tr_t1260, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1260, 0.1).
narrative_ontology:measurement(magn_tr_t1275, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1275, 0.1).
narrative_ontology:measurement(magn_tr_t1300, magna_carta_clause_39__feudal_prerogative_reading, theater_ratio, 1300, 0.1).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1215, 0.6).
narrative_ontology:measurement(magn_be_t1230, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1230, 0.62).
narrative_ontology:measurement(magn_be_t1245, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1245, 0.64).
narrative_ontology:measurement(magn_be_t1260, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1260, 0.66).
narrative_ontology:measurement(magn_be_t1275, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1275, 0.68).
narrative_ontology:measurement(magn_be_t1300, magna_carta_clause_39__feudal_prerogative_reading, base_extractiveness, 1300, 0.7).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1215, 0.7).
narrative_ontology:measurement(magn_su_t1230, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1230, 0.72).
narrative_ontology:measurement(magn_su_t1245, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1245, 0.74).
narrative_ontology:measurement(magn_su_t1260, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1260, 0.76).
narrative_ontology:measurement(magn_su_t1275, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1275, 0.78).
narrative_ontology:measurement(magn_su_t1300, magna_carta_clause_39__feudal_prerogative_reading, suppression_requirement, 1300, 0.8).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_clause_39__feudal_prerogative_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__liberal_due_process_reading).
narrative_ontology:affects_constraint(magna_carta_clause_39__feudal_prerogative_reading, magna_carta_clause_39__originalist_limitation_reading).

% DUAL FORMULATION NOTE:
% This is one of three distinct readings of Magna Carta Clause 39, each modeled as a separate constraint due to differing ε values and structural implications. This 'feudal prerogative' reading emphasizes the narrow, hierarchical nature of the original clause.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
