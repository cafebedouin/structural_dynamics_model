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
 *   human_readable: Magna Carta (1215) - Baronial Privilege Reading
 *   domain: constitutional_law/legal_history/political_theory
 *
 * SUMMARY:
 *   This constraint story models Magna Carta (1215) as a feudal contract
 *   primarily benefiting landowning barons, limiting the arbitrary power of
 *   King John. The definition of 'free men' is strictly interpreted as the
 *   contracting parties, excluding commoners, women, and non-landowners from
 *   its direct protections. The constraint functioned as a 'Tangled Rope,'
 *   coordinating the relationship between the King and his powerful vassals
 *   while simultaneously extracting from the King's absolute authority and
 *   suppressing the rights of the majority of the population through
 *   exclusion. The metrics reflect its operation in its historical context,
 *   with a declining extractiveness and rising theater ratio over centuries
 *   as its direct feudal function atrophied.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(magna_carta_1215__baronial_privilege_reading, 0.7).
domain_priors:suppression_score(magna_carta_1215__baronial_privilege_reading, 0.65).
domain_priors:theater_ratio(magna_carta_1215__baronial_privilege_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, extractiveness, 0.7).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, suppression_requirement, 0.65).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(magna_carta_1215__baronial_privilege_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(magna_carta_1215__baronial_privilege_reading, tangled_rope).
narrative_ontology:human_readable(magna_carta_1215__baronial_privilege_reading, "Magna Carta (1215) - Baronial Privilege Reading").
narrative_ontology:topic_domain(magna_carta_1215__baronial_privilege_reading, "constitutional_law/legal_history/political_theory").

domain_priors:requires_active_enforcement(magna_carta_1215__baronial_privilege_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(magna_carta_1215__baronial_privilege_reading, 'f433a0a3-c7a1-4e1d-8a12-aaaae90b2ee0').
narrative_ontology:cs_kernel_codification('f433a0a3-c7a1-4e1d-8a12-aaaae90b2ee0', fixed_text).
narrative_ontology:cs_authority_grounding('f433a0a3-c7a1-4e1d-8a12-aaaae90b2ee0', lineage).
narrative_ontology:cs_interpretation_layer_present('f433a0a3-c7a1-4e1d-8a12-aaaae90b2ee0').
narrative_ontology:cs_reading_relation('f433a0a3-c7a1-4e1d-8a12-aaaae90b2ee0', magna_carta_1215__universal_rights_reading, forecloses).
narrative_ontology:cs_reading_relation('f433a0a3-c7a1-4e1d-8a12-aaaae90b2ee0', magna_carta_1215__living_document_reading, coexists_with).
narrative_ontology:cs_axiom('f433a0a3-c7a1-4e1d-8a12-aaaae90b2ee0', foundational, feudal_contract_supremacy).
narrative_ontology:cs_axiom_status(feudal_contract_supremacy, holdable).
narrative_ontology:cs_axiom_grounding('f433a0a3-c7a1-4e1d-8a12-aaaae90b2ee0', feudal_contract_supremacy, conventional).
narrative_ontology:cs_axiom('f433a0a3-c7a1-4e1d-8a12-aaaae90b2ee0', foundational, limited_personhood_of_free_men).
narrative_ontology:cs_axiom_status(limited_personhood_of_free_men, holdable).
narrative_ontology:cs_axiom_grounding('f433a0a3-c7a1-4e1d-8a12-aaaae90b2ee0', limited_personhood_of_free_men, conventional).
narrative_ontology:cs_reference_frame('f433a0a3-c7a1-4e1d-8a12-aaaae90b2ee0', feudal_baronial_order_1215).
narrative_ontology:cs_drift_state('f433a0a3-c7a1-4e1d-8a12-aaaae90b2ee0', contemporary_legal_discourse, gap(axiom_overriding, severe, false)).
narrative_ontology:cs_created_at('f433a0a3-c7a1-4e1d-8a12-aaaae90b2ee0', '').
narrative_ontology:cs_kernel_id(magna_carta_1215__baronial_privilege_reading, magna_carta_1215).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(magna_carta_1215__baronial_privilege_reading, landowning_barons).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, king_john).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, commoners).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, women).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, non_landowners).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(magna_carta_1215__baronial_privilege_reading, future_monarchs).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The monarch whose arbitrary power and financial demands led to the charter. He was forced to sign it under duress and sought to annul it, bearing the direct costs of its limitations on his authority.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, king_john, payer,
    institutional, immediate, trapped, national).

% The primary beneficiaries and enforcers of the charter. They negotiated its terms to protect their feudal rights and property from royal overreach, actively ensuring its reissues and adherence. They gained significant legal and political leverage.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, landowning_barons, agenda_setter,
    powerful, generational, constrained, national).

% The vast majority of the population, not considered 'free men' in the context of the charter's primary protections. They were largely excluded from its benefits, remaining subject to feudal lords and royal authority without direct recourse under Magna Carta.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, commoners, excluded,
    powerless, biographical, trapped, local).

% Generally excluded from the protections and rights granted by Magna Carta, which primarily addressed male landholders. Their legal status remained largely tied to their male relatives.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, women, excluded,
    powerless, biographical, trapped, local).

% Individuals without land, including serfs and urban laborers, who were not considered 'free men' and thus did not benefit from the charter's protections against arbitrary seizure or justice.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, non_landowners, excluded,
    powerless, biographical, trapped, local).

% Subsequent kings who inherited the limitations imposed by Magna Carta. While they often sought to reassert royal prerogative, the charter served as a persistent legal and political check on their power.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, future_monarchs, payer,
    institutional, generational, constrained, national).

% Scholars who analyze the historical context and original intent of Magna Carta, interpreting its clauses through the lens of 13th-century feudal law and society.
narrative_ontology:constraint_stakeholder(magna_carta_1215__baronial_privilege_reading, legal_historians, observer,
    analytical, civilizational, analytical, universal).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Established a written framework for feudal obligations and rights between the King and his most powerful vassals, aiming to prevent arbitrary royal power and ensure a degree of stability in the feudal system.
% TRANSFER_FUNCTION: Transferred specific powers and privileges from the King to the landowning barons, limiting royal prerogative in areas like taxation, justice, and feudal dues. It also implicitly transferred the burden of arbitrary rule from barons to commoners by not extending protections to them.
% ABSENT_VOICES: Commoners, women, and non-landowners were entirely absent from the negotiation and were not considered 'free men' in the context of the charter's protections. Their voices would have demanded broader rights and protections, challenging the narrow scope of the document.
% DISAPPEARANCE_RATIONALE: If Magna Carta had never existed or vanished overnight in 1215, the relationship between the King and his barons would have remained far more arbitrary, likely leading to more frequent and severe feudal conflicts, and a different, potentially more absolutist, trajectory for English constitutional development.
% FOUNDING_PROBLEM: King John's arbitrary rule, excessive taxation, and disregard for feudal law, which provoked a rebellion by the landowning barons who sought to codify their traditional rights and limit royal power.
% FOUNDING_PROBLEM_CORROBORATION: Contemporary chronicles, such as Roger of Wendover's 'Flores Historiarum,' and extensive historical analyses by independent legal historians (e.g., William Stubbs, J.C. Holt) corroborate the specific grievances of the barons against King John and the context of the charter's creation. They attest that the specific problem of King John's tyrannical rule is long gone, though its legacy persists.
narrative_ontology:disappearance_verdict(magna_carta_1215__baronial_privilege_reading, world_rearranges).
narrative_ontology:founding_problem_status(magna_carta_1215__baronial_privilege_reading, dead).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(magna_carta_1215__baronial_privilege_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(magna_carta_1215__baronial_privilege_reading, 'none', 1).
narrative_ontology:epsilon_provenance(magna_carta_1215__baronial_privilege_reading, 0.7, 'gemini-2.5-flash', 'none', direct).

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
 *   The high extractiveness (0.7) reflects the significant reduction in the King's arbitrary power over the barons. Suppression (0.65) is high due to the active enforcement required by the barons to maintain the charter's provisions against royal resistance, and the structural suppression of rights for excluded groups. The low initial theater ratio (0.1) indicates it was a genuinely functional and enforced document in its early centuries. Accessibility collapse is high (0.8) for the King, whose options were severely curtailed, and for commoners, whose alternatives for legal recourse were collapsed by their exclusion. Resistance (0.5) was moderate, coming from both the King's attempts to evade it and, eventually, from commoners seeking broader rights.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the landowning barons, Magna Carta was a vital 'Rope' that established order and protected their rights. From the King's perspective, it was a 'Snare' that severely curtailed his traditional authority. From the perspective of the excluded commoners, it was a 'Snare' that formalized their lack of rights relative to the privileged class. The engine's per-seat classification will capture these divergences.
 *
 * DIRECTIONALITY LOGIC:
 *   The landowning barons are the clear beneficiaries and agenda-setters, gaining codified rights and protections. King John and subsequent monarchs are the payers, as their power was directly curtailed. Commoners, women, and non-landowners are victims by exclusion; while not directly paying a 'fee,' they bore the cost of not being protected by the charter's provisions, effectively subsidizing the baronial privilege. Legal historians act as analytical observers.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    scope_of_free_men_definition,
    'Is the definition of ''free men'' in Magna Carta strictly limited to landowning barons and their direct vassals, or does it implicitly contain a broader, proto-universalist meaning?',
    'Detailed linguistic and legal analysis of 13th-century English legal texts and social structures, comparing the usage of ''liber homo'' (free man) in various contexts beyond Magna Carta.',
    'If strictly limited, this reading''s classification as a Tangled Rope for baronial privilege is reinforced. If a broader implicit meaning is found, the constraint''s extractiveness from commoners might be re-evaluated downward, and its claimed type might shift towards a more inclusive (though still limited) Rope.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(scope_of_free_men_definition, empirical, 'Ambiguity in the historical definition of ''free men'' and its implications for the charter''s scope.').

omega_variable(
    feudal_contract_vs_constitutional_precedent,
    'Is Magna Carta fundamentally a specific feudal contract addressing 13th-century grievances, or does it inherently possess qualities of a foundational constitutional document with transhistorical principles?',
    'Comparative legal history examining how similar feudal documents evolved versus those that became constitutional cornerstones, and analysis of its subsequent reception and reinterpretation in later centuries.',
    'If primarily a feudal contract, this reading''s focus on specific, limited protections is strengthened. If a foundational constitutional document, its ''Tangled Rope'' classification might be seen as a temporary phase in a longer ''Scaffold'' or ''Rope'' trajectory, even if its initial application was narrow.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(feudal_contract_vs_constitutional_precedent, conceptual, 'The conceptual framing of Magna Carta as either a specific contract or a broader constitutional precedent.').

omega_variable(
    mandatrophy_of_feudal_function,
    'To what extent did the original feudal coordination function of Magna Carta become obsolete, leading to its persistence primarily as a symbolic or theatrical constraint?',
    'Analysis of the decline of feudalism and the rise of parliamentary sovereignty, tracking when specific clauses ceased to be actively enforced in their original context and became primarily cited for symbolic value.',
    'If the feudal function became entirely obsolete, the constraint''s later classification would shift towards a ''Piton'' as its theatrical ratio increased, indicating maintenance by inertia rather than active function. This reading''s high initial extractiveness would then be seen as a historical snapshot of a constraint that later atrophied.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(mandatrophy_of_feudal_function, empirical, 'The historical process of the constraint''s original function becoming obsolete.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(magna_carta_1215__baronial_privilege_reading, 1215, 2015).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(magn_tr_t1215, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1215, 0.1).
narrative_ontology:measurement(magn_tr_t1250, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1250, 0.1).
narrative_ontology:measurement(magn_tr_t1300, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1300, 0.15).
narrative_ontology:measurement(magn_tr_t1400, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1400, 0.2).
narrative_ontology:measurement(magn_tr_t1600, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1600, 0.3).
narrative_ontology:measurement(magn_tr_t1800, magna_carta_1215__baronial_privilege_reading, theater_ratio, 1800, 0.5).
narrative_ontology:measurement(magn_tr_t2015, magna_carta_1215__baronial_privilege_reading, theater_ratio, 2015, 0.7).

% Extraction over time
narrative_ontology:measurement(magn_be_t1215, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1215, 0.7).
narrative_ontology:measurement(magn_be_t1250, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1250, 0.65).
narrative_ontology:measurement(magn_be_t1300, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1300, 0.6).
narrative_ontology:measurement(magn_be_t1400, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1400, 0.55).
narrative_ontology:measurement(magn_be_t1600, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1600, 0.45).
narrative_ontology:measurement(magn_be_t1800, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 1800, 0.3).
narrative_ontology:measurement(magn_be_t2015, magna_carta_1215__baronial_privilege_reading, base_extractiveness, 2015, 0.1).

% Suppression requirement over time
narrative_ontology:measurement(magn_su_t1215, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1215, 0.65).
narrative_ontology:measurement(magn_su_t1250, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1250, 0.6).
narrative_ontology:measurement(magn_su_t1300, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1300, 0.55).
narrative_ontology:measurement(magn_su_t1400, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1400, 0.5).
narrative_ontology:measurement(magn_su_t1600, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1600, 0.4).
narrative_ontology:measurement(magn_su_t1800, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 1800, 0.25).
narrative_ontology:measurement(magn_su_t2015, magna_carta_1215__baronial_privilege_reading, suppression_requirement, 2015, 0.1).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(magna_carta_1215__baronial_privilege_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__universal_rights_reading).
narrative_ontology:affects_constraint(magna_carta_1215__baronial_privilege_reading, magna_carta_1215__living_document_reading).

% DUAL FORMULATION NOTE:
% Magna Carta (1215) is a kernel with multiple structurally distinct readings. This story represents the 'baronial privilege' reading, which interprets the charter as a feudal contract for landowning barons. Other readings (universal rights, living document) are modeled as separate constraints due to their differing epsilon values and stakeholder structures.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
