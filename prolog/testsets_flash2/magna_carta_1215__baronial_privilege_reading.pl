% ============================================================================
% CONSTRAINT STORY: magna_carta_1215__baronial_privilege_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_magna_carta_1215__baronial_privilege_reading, []).

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
 *   constraint_id: magna_carta_1215__baronial_privilege_reading
 *   human_readable: Magna Carta (1215) - Baronial Privilege Reading
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint story represents a 'baronial privilege' reading of Magna
 *   Carta (1215), where 'free men' is interpreted narrowly to refer primarily
 *   to landowning barons. The charter is understood as a feudal contract
 *   designed to limit King John's arbitrary power over his vassals, rather
 *   than a document establishing universal rights. Its protections were
 *   limited to the contracting parties, with little direct benefit for
 *   commoners or other non-landowning subjects. This reading emphasizes the
 *   historical context and the specific political dynamics of 13th-century
 *   England.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, 0.25).
domain_priors:suppression_score(magna_carta_1215__baronial_privilege_reading, 0.4).
domain_priors:theater_ratio(magna_carta_1215__baronial_privilege_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, resistance, 0.1).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__baronial_privilege_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__baronial_privilege_reading, "Magna Carta (1215) - Baronial Privilege Reading").
narrative_ontology:topic_domain(magna_carta_1215__baronial_privilege_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__baronial_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__baronial_privilege_reading, 'd53a8251-73ba-4670-ac73-ae1ae3422aa8').
narrative_ontology:cs_kernel_codification('d53a8251-73ba-4670-ac73-ae1ae3422aa8', fixed_text).
narrative_ontology:cs_authority_grounding('d53a8251-73ba-4670-ac73-ae1ae3422aa8', lineage).
narrative_ontology:cs_interpretation_layer_present('d53a8251-73ba-4670-ac73-ae1ae3422aa8').
narrative_ontology:cs_reading_relation('d53a8251-73ba-4670-ac73-ae1ae3422aa8', magna_carta_1215__universal_rights_reading, coexists_with).
narrative_ontology:cs_reading_relation('d53a8251-73ba-4670-ac73-ae1ae3422aa8', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('d53a8251-73ba-4670-ac73-ae1ae3422aa8', foundational, feudal_contract_supremacy).
narrative_ontology:cs_axiom_status(feudal_contract_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('d53a8251-73ba-4670-ac73-ae1ae3422aa8', feudal_contract_supremacy, conventional).
narrative_ontology:cs_axiom('d53a8251-73ba-4670-ac73-ae1ae3422aa8', foundational, free_men_as_landowning_barons).
narrative_ontology:cs_axiom_status(free_men_as_landowning_barons, holdable).
narrative_ontology:cs_axiom_grounding('d53a8251-73ba-4670-ac73-ae1ae3422aa8', free_men_as_landowning_barons, conventional).
narrative_ontology:cs_reference_frame('d53a8251-73ba-4670-ac73-ae1ae3422aa8', feudal_baronial_privilege).
narrative_ontology:cs_drift_state('d53a8251-73ba-4670-ac73-ae1ae3422aa8', contemporary_constitutional_discourse, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('d53a8251-73ba-4670-ac73-ae1ae3422aa8', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__baronial_privilege_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, landowning_barons).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, king_john).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary beneficiaries of the charter, securing specific feudal rights and protections against arbitrary royal power. Their power derived from land ownership and military capacity, allowing them to negotiate with the King. Exit options were limited to rebellion or submission, both with high costs.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, landowning_barons, beneficiary,
    powerful, generational, constrained, national).

% The target of the constraint, forced to concede specific rights and limit his arbitrary power over the barons. His options were to accept the charter or face continued civil war and potential loss of his throne. The constraint extracted royal prerogative.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, king_john, payer,
    institutional, biographical, constrained, national).

% Structurally excluded from the protections and benefits of the charter, as 'free men' was interpreted narrowly to mean landowning barons. Their rights and protections remained subject to feudal lords and royal prerogative without direct recourse under Magna Carta.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, commoners_non_landowners, excluded,
    powerless, immediate, trapped, local).

% Analyze the historical context and original intent of Magna Carta, interpreting 'free men' as a specific class of feudal lords. They document the charter's immediate impact and its limited scope within 13th-century English society.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a formal agreement between the King and his most powerful vassals, codifying feudal rights and obligations to prevent arbitrary royal overreach and ensure a degree of stability in the realm.
% TRANSFER_FUNCTION: Transferred specific feudal rights and protections from the King's absolute prerogative to the landowning barons, limiting royal power and securing their property and legal standing.
% ABSENT_VOICES: Commoners, women, and non-landowners were largely absent from the negotiations and were not direct beneficiaries of the charter's protections. They would have argued for broader application of 'free men' and due process, but their voices were not structurally included in the feudal contract.
% DISAPPEARANCE_RATIONALE: If Magna Carta had never existed, the relationship between the English monarchy and its nobility would likely have remained more arbitrary, potentially leading to more frequent and severe conflicts over royal power and feudal rights. The subsequent development of English common law and constitutionalism would have taken a different path.
% FOUNDING_PROBLEM: King John's arbitrary rule, excessive taxation, and disregard for feudal customs led to widespread discontent and rebellion among the English barons, threatening the stability of the kingdom.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and political theorists widely corroborate that King John's specific abuses of power were the direct cause of the charter. While the *spirit* of limiting arbitrary power persists, the specific feudal problems of 1215 are long resolved; the arrangement persists due to its symbolic and historical weight, not its original function. Independent historical scholarship from outside the benefiting parties (e.g., academic historians) supports this view.
narrative_ontology:disappearance_verdict(magna_carta_1215__baronial_privilege_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__baronial_privilege_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__baronial_privilege_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_1215__baronial_privilege_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__baronial_privilege_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__baronial_privilege_reading_tests).
:- end_tests(magna_carta_1215__baronial_privilege_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is moderate (0.25) as it primarily limited the King's ability to extract resources and loyalty arbitrarily, but did not create a broad extractive mechanism for the barons over others. Suppression (0.4) reflects the need for continued enforcement against royal attempts to disregard the charter, but also the limited scope of its application. Theater ratio is low (0.1) as the charter's provisions were directly relevant and functional for its intended beneficiaries at the time. Accessibility collapse is high (0.7) because for the intended beneficiaries, the charter provided a clear, if limited, set of protections that were otherwise unavailable. Resistance is low (0.1) because the charter was a negotiated settlement, not a widely resisted imposition by the beneficiaries.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the landowning barons, Magna Carta was a vital 'rope' that coordinated their collective action against an overreaching monarch, securing essential feudal liberties. From the King's perspective, it was a 'snare' that curtailed his power. From the perspective of commoners, it was largely irrelevant, a 'mountain' of privilege that did not affect their daily lives.
 *
 * DIRECTIONALITY LOGIC:
 *   The landowning barons are the clear beneficiaries, gaining specific protections and limitations on royal power. King John is the target, having his prerogative constrained. Commoners and non-landowners are excluded, as the charter's benefits did not extend to them under this reading. Legal historians act as observers, analyzing the document's original intent and impact.
 *
 * MANDATROPHY ANALYSIS:
 *   This reading prevents mislabeling the charter as a universal rights document (a 'mountain' or 'rope' for all) by focusing on its original, limited scope. The 'dead' status of the founding problem, combined with the 'world_rearranges' disappearance verdict, suggests a historical artifact whose original function has atrophied, but whose symbolic weight continues to influence later constitutional development, making it a candidate for a 'piton' in later readings, but a functional 'rope' in its original context.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_free_men,
    'Was ''free men'' in Magna Carta intended to apply beyond landowning barons to a broader class of subjects, even if not all commoners?',
    'Further historical linguistic analysis of 13th-century legal terminology and social structures, or discovery of new contemporary interpretations of the phrase.',
    'If ''free men'' was found to have a broader original meaning, the victim/beneficiary set would expand, increasing the charter''s original coordination function and potentially reclassifying it as a broader ''rope'' or ''tangled_rope'' even in its original context.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(scope_of_free_men, empirical, 'Ambiguity regarding the original scope of ''free men'' in Magna Carta.').

omega_variable(
    kernel_reading_divergence,
    'How does this ''baronial privilege'' reading structurally differ from the ''universal rights'' and ''living document'' readings of Magna Carta?',
    'Comparative analysis of the beneficiary/victim sets, enforcement mechanisms, and claimed coordination functions across all three readings.',
    'The ''baronial privilege'' reading emphasizes a narrow, feudal ''rope'' or ''tangled_rope'' for a specific class, while ''universal rights'' would claim a ''mountain'' or ''rope'' for all persons, and ''living document'' would describe an evolving ''scaffold'' or ''piton''. The divergence highlights how different interpretations of the same kernel yield distinct constraints.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_divergence, conceptual, 'This constraint is one reading of the Magna Carta kernel; other readings would yield different classifications.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__baronial_privilege_reading, 1215, 1215).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1215, 0.1).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1215, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1215, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
