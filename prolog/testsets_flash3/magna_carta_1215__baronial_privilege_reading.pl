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
 *   constraint_id: magna_carta_1215__baronial_privilege_reading
 *   human_readable: Magna Carta (1215) as Baronial Privilege
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint story interprets Magna Carta (1215) strictly as a feudal
 *   contract primarily benefiting landowning barons, limiting the power of
 *   King John. The 'free men' mentioned in the charter are understood to
 *   refer to this specific class, not to all persons. The protections
 *   afforded are limited to the contracting parties, reflecting the political
 *   and social structure of 13th-century England. This reading emphasizes the
 *   document's historical context and its role in resolving a specific
 *   dispute between the King and his vassals, rather than as a universal
 *   declaration of rights.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, 0.3).
domain_priors:suppression_score(magna_carta_1215__baronial_privilege_reading, 0.6).
domain_priors:theater_ratio(magna_carta_1215__baronial_privilege_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, extractiveness, 0.3).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, accessibility_collapse, 0.7).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, resistance, 0.2).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__baronial_privilege_reading, rope).
narrative_ontology:human_readable(magna_carta_1215__baronial_privilege_reading, "Magna Carta (1215) as Baronial Privilege").
narrative_ontology:topic_domain(magna_carta_1215__baronial_privilege_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__baronial_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__baronial_privilege_reading, '509bf120-0a2a-4b7e-a27c-ff70cfff4a56').
narrative_ontology:cs_kernel_codification('509bf120-0a2a-4b7e-a27c-ff70cfff4a56', fixed_text).
narrative_ontology:cs_authority_grounding('509bf120-0a2a-4b7e-a27c-ff70cfff4a56', lineage).
narrative_ontology:cs_interpretation_layer_present('509bf120-0a2a-4b7e-a27c-ff70cfff4a56').
narrative_ontology:cs_reading_relation('509bf120-0a2a-4b7e-a27c-ff70cfff4a56', magna_carta_1215__universal_rights_reading, influences).
narrative_ontology:cs_reading_relation('509bf120-0a2a-4b7e-a27c-ff70cfff4a56', magna_carta_1215__living_document_reading, influences).
narrative_ontology:cs_axiom('509bf120-0a2a-4b7e-a27c-ff70cfff4a56', foundational, feudal_contract_supremacy).
narrative_ontology:cs_axiom_status(feudal_contract_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('509bf120-0a2a-4b7e-a27c-ff70cfff4a56', feudal_contract_supremacy, conventional).
narrative_ontology:cs_axiom('509bf120-0a2a-4b7e-a27c-ff70cfff4a56', foundational, free_men_as_landowners).
narrative_ontology:cs_axiom_status(free_men_as_landowners, holdable).
narrative_ontology:cs_axiom_grounding('509bf120-0a2a-4b7e-a27c-ff70cfff4a56', free_men_as_landowners, empirically_contingent).
narrative_ontology:cs_reference_frame('509bf120-0a2a-4b7e-a27c-ff70cfff4a56', feudal_contract_tradition).
narrative_ontology:cs_drift_state('509bf120-0a2a-4b7e-a27c-ff70cfff4a56', contemporary_constitutional_discourse, gap(practice_drift, severe, true)).
narrative_ontology:cs_created_at('509bf120-0a2a-4b7e-a27c-ff70cfff4a56', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__baronial_privilege_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, landowning_barons).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, king_john).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, future_monarchs).
narrative_ontology:constraint_vindicates(magna_carta_1215__baronial_privilege_reading, feudal_contract_supremacy).
narrative_ontology:constraint_vindicates(magna_carta_1215__baronial_privilege_reading, limited_monarchy_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The primary beneficiaries of the charter, securing specific feudal rights and protections against arbitrary royal power. Their power derived from land ownership and military capacity, allowing them to compel the King to sign. Exit options were limited to rebellion or submission, both high-cost.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, landowning_barons, beneficiary,
    powerful, generational, constrained, national).

% The primary target of the constraint, forced to concede specific powers and revenues to the barons. His authority was curtailed by the charter, which he initially resisted but was compelled to accept due to baronial military strength. His exit was effectively submission or civil war.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, king_john, payer,
    institutional, biographical, trapped, national).

% Not directly protected by the charter's provisions, which primarily addressed the rights of 'free men' (interpreted as landowning barons). They had no voice in its creation and gained no explicit new rights, remaining subject to feudal lords and royal authority without the specific protections afforded to the barons.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, commoners_non_landowners, excluded,
    powerless, immediate, trapped, local).

% Inherited the limitations placed on royal power by Magna Carta. While they could attempt to reassert authority, the charter established a precedent for constitutional limits, influencing subsequent reigns and requiring ongoing negotiation or enforcement.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, future_monarchs, payer,
    institutional, generational, constrained, national).

% Analyze Magna Carta within its historical context, emphasizing its feudal origins and the specific social structure of 13th-century England. They interpret 'free men' as a narrow class, distinct from later universalist readings.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, legal_historians, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a formal agreement between the King and his most powerful vassals, codifying feudal rights and obligations to prevent arbitrary royal overreach and stabilize the kingdom after a period of conflict.
% TRANSFER_FUNCTION: Transferred specific rights and immunities from the King to the landowning barons, limiting royal taxation, judicial power, and feudal levies, thereby shifting wealth and authority to the baronial class.
% ABSENT_VOICES: Commoners, women, and non-landowners were entirely absent from the negotiation and were not direct beneficiaries of the charter's protections. They would have advocated for broader rights and protections beyond the feudal contract.
% DISAPPEARANCE_RATIONALE: If Magna Carta had never existed, the trajectory of English constitutional development would be fundamentally different. The precedent for limiting royal power, even if initially narrow, would be absent, potentially leading to a more absolute monarchy or different forms of resistance and political organization.
% FOUNDING_PROBLEM: King John's arbitrary rule, excessive taxation, and abuses of feudal custom led to widespread discontent and rebellion among the English barons, threatening the stability of the kingdom.
% FOUNDING_PROBLEM_CORROBORATION: Legal historians and political theorists widely corroborate that the specific abuses of King John and the feudal context that gave rise to Magna Carta are no longer live problems. The original problem is dead, but the document's symbolic and legal legacy persists, interpreted in new contexts.
narrative_ontology:disappearance_verdict(magna_carta_1215__baronial_privilege_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__baronial_privilege_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__baronial_privilege_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(magna_carta_1215__baronial_privilege_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__baronial_privilege_reading, 0.3, 'gemini-2.5-flash', 'none', direct).

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
 *   Extractiveness is moderate (0.3) as it primarily limited the King's ability to extract resources from the barons, rather than extracting from them. Suppression is moderate (0.6) because the King's power was actively curtailed by the threat of baronial rebellion, and the charter required ongoing enforcement to hold. Theater ratio is low (0.1) as the document's initial function was direct and practical, not performative. Accessibility collapse is high (0.7) because the charter's terms, once understood, clearly defined the limits of royal power for the barons. Resistance is low (0.2) from the beneficiaries, as they were the ones imposing the constraint.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the landowning barons, Magna Carta was a necessary coordination mechanism to curb royal tyranny and secure their traditional rights. From King John's perspective, it was an imposed extraction. Legal historians, as observers, analyze this dynamic without being subject to its direct effects.
 *
 * DIRECTIONALITY LOGIC:
 *   The landowning barons are the clear beneficiaries, gaining specific rights and protections (low d). King John is the target, having his arbitrary powers curtailed (high d). Future monarchs are also targets, inheriting these limitations. Commoners and non-landowners are excluded, neither directly benefiting nor being directly targeted by the specific provisions of this reading, though their overall situation is shaped by the feudal system it reinforces.
 *
 * MANDATROPHY ANALYSIS:
 *   The founding problem (King John's abuses) is 'dead,' yet the document persists. This suggests a potential for mandatrophy, where the constraint's original function has atrophied but it remains due to its symbolic power or reinterpretation. However, in this 'baronial privilege' reading, the constraint's direct function was limited to the feudal context, which is no longer active. The persistence of Magna Carta in later legal systems is due to other readings, not this one, which is why it computes as a Rope (a coordination mechanism that solved a genuine problem for its specific beneficiaries at the time) rather than a Piton (a degraded constraint persisting by inertia for no clear benefit).
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_free_men,
    'Does ''free men'' in Magna Carta refer exclusively to landowning barons, or does it encompass a broader class of individuals in 13th-century England?',
    'Further historical and linguistic analysis of 13th-century legal terminology and social structures, including contemporary commentaries and legal records.',
    'If ''free men'' is found to include a broader class, the constraint''s beneficiary set expands, potentially shifting its classification towards a broader ''rope'' or even a nascent ''scaffold'' for a wider population, rather than a narrow ''rope'' for barons.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_free_men, empirical, 'Ambiguity in the historical interpretation of ''free men'' in Magna Carta.').

omega_variable(
    magna_carta_framing,
    'Is Magna Carta best understood as a specific feudal contract, or as a foundational document for broader constitutional principles?',
    'Conceptual analysis of legal and political theory, examining the role of historical context versus evolving interpretive traditions in constitutional development.',
    'Framing it as a feudal contract (this reading) limits its direct applicability to modern constitutional law, while framing it as a foundational document (e.g., ''universal_rights_reading'') would imply a much higher extractiveness and suppression against arbitrary power, and a broader beneficiary set.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(magna_carta_framing, conceptual, 'Conceptual framing of Magna Carta''s primary nature and purpose.').


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
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1215, 0.3).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1215, 0.6).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__baronial_privilege_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__universal_rights_reading).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__living_document_reading).

% DUAL FORMULATION NOTE:
% Magna Carta (1215) is a kernel with multiple readings. This 'baronial_privilege_reading' focuses on its original feudal context and limited scope. It influences, but does not foreclose, later 'universal_rights_reading' and 'living_document_reading' interpretations, which expand its scope and meaning.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
