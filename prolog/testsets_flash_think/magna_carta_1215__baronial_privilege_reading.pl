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
 *   constraint_id: magna_carta_1215__baronial_privilege_reading
 *   human_readable: Magna Carta (1215) as Baronial Privilege
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint story models the Magna Carta of 1215 through the lens of
 *   its original historical context: a feudal contract primarily concerned
 *   with the rights and obligations between King John and his landowning
 *   barons. In this reading, the term 'free men' is understood narrowly,
 *   limiting the document's protections to the contracting parties and
 *   excluding commoners, women, and non-landowners. The constraint functions
 *   as a coordination mechanism for the barons to limit royal power, while
 *   simultaneously extracting specific arbitrary powers from the King.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, 0.65).
domain_priors:suppression_score(magna_carta_1215__baronial_privilege_reading, 0.55).
domain_priors:theater_ratio(magna_carta_1215__baronial_privilege_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, extractiveness, 0.65).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0.55).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__baronial_privilege_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__baronial_privilege_reading, "Magna Carta (1215) as Baronial Privilege").
narrative_ontology:topic_domain(magna_carta_1215__baronial_privilege_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__baronial_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__baronial_privilege_reading, '55fde5a7-fd02-460d-b280-fd5bab00b23d').
narrative_ontology:cs_kernel_codification('55fde5a7-fd02-460d-b280-fd5bab00b23d', fixed_text).
narrative_ontology:cs_authority_grounding('55fde5a7-fd02-460d-b280-fd5bab00b23d', lineage).
narrative_ontology:cs_interpretation_layer_present('55fde5a7-fd02-460d-b280-fd5bab00b23d').
narrative_ontology:cs_reading_relation('55fde5a7-fd02-460d-b280-fd5bab00b23d', magna_carta_1215__universal_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('55fde5a7-fd02-460d-b280-fd5bab00b23d', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('55fde5a7-fd02-460d-b280-fd5bab00b23d', foundational, feudal_contract_supremacy).
narrative_ontology:cs_axiom_status(feudal_contract_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('55fde5a7-fd02-460d-b280-fd5bab00b23d', feudal_contract_supremacy, conventional).
narrative_ontology:cs_axiom('55fde5a7-fd02-460d-b280-fd5bab00b23d', foundational, free_men_limited_to_landowners).
narrative_ontology:cs_axiom_status(free_men_limited_to_landowners, holdable).
narrative_ontology:cs_axiom_grounding('55fde5a7-fd02-460d-b280-fd5bab00b23d', free_men_limited_to_landowners, conventional).
narrative_ontology:cs_reference_frame('55fde5a7-fd02-460d-b280-fd5bab00b23d', original_feudal_contract_intent).
narrative_ontology:cs_drift_state('55fde5a7-fd02-460d-b280-fd5bab00b23d', contemporary_constitutional_discourse, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('55fde5a7-fd02-460d-b280-fd5bab00b23d', '').
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

% The monarch whose arbitrary power and feudal abuses were directly curtailed by the Magna Carta. He was forced to sign it and resisted its enforcement, viewing it as an infringement on his divine right to rule. The constraint extracted specific powers and revenues from him.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, king_john, payer,
    institutional, biographical, constrained, national).

% The primary architects and beneficiaries of the Magna Carta in this reading. They coordinated to force the King to accept limitations on his power, securing their feudal rights, property, and due process within the feudal system. Their exit option was civil war, which they pursued when the charter was repudiated.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, landowning_barons, agenda_setter,
    organized, generational, constrained, regional).
narrative_ontology:stakeholder_secondary_role(magna_carta_1215__baronial_privilege_reading, landowning_barons, beneficiary).

% The vast majority of the population, including serfs and non-landowning freemen, who were largely outside the direct protections of the Magna Carta in its original feudal context. The document did not significantly alter their legal status or provide them with new rights.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, commoners, excluded,
    powerless, immediate, trapped, local).

% Individuals, particularly women and those without land, who were explicitly or implicitly excluded from the protections and privileges granted by the Magna Carta to 'free men' (interpreted as landowning males). Their voices were absent from its creation and its direct benefits.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, women_non_landowners, excluded,
    powerless, immediate, trapped, local).

% Scholars who analyze the historical context, original intent, and legal impact of the Magna Carta, often distinguishing its feudal origins from later interpretations. They provide an analytical perspective on its narrow application in 1215.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, legal_historians, observer,
    analytical, civilizational, analytical, universal).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(magna_carta_1215__baronial_privilege_reading, landowning_barons).
narrative_ontology:fixing_cost_class(magna_carta_1215__baronial_privilege_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a legal framework that limited the King's arbitrary power, defined feudal rights and obligations, and secured specific protections for landowning barons against royal overreach, thereby stabilizing the feudal hierarchy.
% TRANSFER_FUNCTION: Transfers arbitrary power and revenue-raising capacity from the King to a codified legal framework, securing specific feudal rights, property, and due process for the landowning barons.
% ABSENT_VOICES: Commoners, women, and non-landowners were structurally excluded from the negotiations and the direct benefits of the charter. They would have advocated for broader protections and a more inclusive definition of 'free men' if their voices had been present.
% DISAPPEARANCE_RATIONALE: If the Magna Carta and its principles had vanished overnight in 1215, the trajectory of English constitutional law and the development of parliamentary power would have been fundamentally altered. The King's arbitrary power would have remained unchecked, likely leading to continued feudal conflict and a different path for legal and political development.
% FOUNDING_PROBLEM: King John's arbitrary and oppressive rule, including excessive taxation, seizure of property, and disregard for established feudal law, which led to widespread discontent and rebellion among the English barons.
% FOUNDING_PROBLEM_CORROBORATION: Historical chronicles from the period (e.g., Roger of Wendover, Matthew Paris), contemporary legal documents, and centuries of independent legal and historical scholarship corroborate the specific abuses of King John and the barons' grievances, supporting the view that the immediate founding problem is long resolved.
narrative_ontology:disappearance_verdict(magna_carta_1215__baronial_privilege_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__baronial_privilege_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__baronial_privilege_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(magna_carta_1215__baronial_privilege_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__baronial_privilege_reading, 0.65, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(magna_carta_1215__baronial_privilege_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(magna_carta_1215__baronial_privilege_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The constraint is classified as a Tangled Rope because it served a genuine coordination function for the barons (securing their rights and limiting the King's arbitrary rule) while simultaneously extracting power from the King. Its persistence required active enforcement by the barons, often through military means. Extractiveness is moderate-high, reflecting the significant curtailment of royal prerogative. Suppression is moderate, as it actively suppressed the King's ability to act arbitrarily. Theater ratio is low, as the document was a serious legal instrument, not primarily performative. Resistance was high, as King John immediately repudiated it, leading to civil war.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the landowning barons, the Magna Carta was a vital coordination mechanism that secured their rights and brought stability to the feudal system. From King John's perspective, it was an illegitimate imposition that severely curtailed his authority. For commoners, it was largely irrelevant to their daily lives. The engine's per-seat classification would reflect these divergent experiences.
 *
 * DIRECTIONALITY LOGIC:
 *   The landowning barons are the primary beneficiaries and agenda-setters, gaining secured rights and limitations on the King's power. King John is the primary victim, as the constraint directly extracts arbitrary power and revenue from him. Commoners and non-landowning individuals are excluded, receiving no direct benefit or bearing no direct cost from this specific feudal contract, as it did not apply to them.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_free_men_ambiguity,
    'Does the term ''free men'' in Magna Carta refer exclusively to landowning barons, or does it implicitly lay groundwork for broader, more universal rights?',
    'Analysis of contemporary legal dictionaries, feudal charters, and social structures of 13th-century England, alongside the explicit exclusions of other groups within the document.',
    'If ''free men'' is strictly limited to landowning barons, this reading''s classification as a Tangled Rope (benefiting a narrow group, extracting from the King) is reinforced. If it''s found to have an implicit broader scope, it would shift towards a more Rope-like classification for a wider set of beneficiaries, even if not fully universal.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_free_men_ambiguity, empirical, 'Ambiguity regarding the precise scope of ''free men'' in the original text.').

omega_variable(
    historical_vs_modern_interpretation,
    'To what extent should the original historical context and intent of Magna Carta (as a feudal contract) constrain its interpretation in modern constitutional law?',
    'Legal philosophical debate and judicial precedent regarding originalism vs. living constitutionalism. This is a conceptual choice, not an empirical one.',
    'If original intent is strictly binding, this reading remains the primary lens, and other readings are seen as later impositions. If modern interpretation is privileged, this reading becomes a historical artifact, and the ''universal_rights_reading'' or ''living_document_reading'' would gain precedence, leading to different classifications for the contemporary constraint.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_vs_modern_interpretation, conceptual, 'The tension between historical context and contemporary legal interpretation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__baronial_privilege_reading, 1215, 1250).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1225, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1225, 0.09).
narrative_ontology:measurement(magn_tr_t1235, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1235, 0.08).
narrative_ontology:measurement(magn_tr_t1250, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1250, 0.07).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1215, 0.65).
narrative_ontology:measurement(magn_be_t1225, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1225, 0.62).
narrative_ontology:measurement(magn_be_t1235, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1235, 0.6).
narrative_ontology:measurement(magn_be_t1250, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1250, 0.58).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1215, 0.55).
narrative_ontology:measurement(magn_su_t1225, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1225, 0.52).
narrative_ontology:measurement(magn_su_t1235, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1235, 0.5).
narrative_ontology:measurement(magn_su_t1250, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1250, 0.48).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */


/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
