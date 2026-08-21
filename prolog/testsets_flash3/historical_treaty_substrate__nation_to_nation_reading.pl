% ============================================================================
% CONSTRAINT STORY: historical_treaty_substrate__nation_to_nation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_historical_treaty_substrate__nation_to_nation_reading, []).

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
 *   constraint_id: historical_treaty_substrate__nation_to_nation_reading
 *   human_readable: Historical Treaty Substrate (Nation-to-Nation Reading)
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This constraint represents the 'nation-to-nation' reading of historical
 *   treaties, where they are understood as ongoing international agreements
 *   between sovereign equals, requiring continuous consent and adherence to
 *   modern treaty law. This reading emphasizes Indigenous sovereignty and
 *   limits the settler state's unilateral actions, particularly regarding
 *   resource extraction. This is one reading of the
 *   'historical_treaty_substrate' kernel, contrasting with
 *   'extinguishment_reading' and 'stewardship_reading'.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, 0.25).
domain_priors:suppression_score(historical_treaty_substrate__nation_to_nation_reading, 0.4).
domain_priors:theater_ratio(historical_treaty_substrate__nation_to_nation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, extractiveness, 0.25).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0.4).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, accessibility_collapse, 0.3).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__nation_to_nation_reading, rope).
narrative_ontology:human_readable(historical_treaty_substrate__nation_to_nation_reading, "Historical Treaty Substrate (Nation-to-Nation Reading)").
narrative_ontology:topic_domain(historical_treaty_substrate__nation_to_nation_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__nation_to_nation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__nation_to_nation_reading, 'd995b80c-e4fb-4e0e-bb0a-d08bf0156217').
narrative_ontology:cs_kernel_codification('d995b80c-e4fb-4e0e-bb0a-d08bf0156217', fixed_text).
narrative_ontology:cs_authority_grounding('d995b80c-e4fb-4e0e-bb0a-d08bf0156217', lineage).
narrative_ontology:cs_interpretation_layer_present('d995b80c-e4fb-4e0e-bb0a-d08bf0156217').
narrative_ontology:cs_reading_relation('d995b80c-e4fb-4e0e-bb0a-d08bf0156217', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('d995b80c-e4fb-4e0e-bb0a-d08bf0156217', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('d995b80c-e4fb-4e0e-bb0a-d08bf0156217', foundational, indigenous_nations_possess_inherent_sovereignty).
narrative_ontology:cs_axiom_status(indigenous_nations_possess_inherent_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('d995b80c-e4fb-4e0e-bb0a-d08bf0156217', indigenous_nations_possess_inherent_sovereignty, deontological).
narrative_ontology:cs_axiom('d995b80c-e4fb-4e0e-bb0a-d08bf0156217', foundational, treaties_are_living_documents_subject_to_international_law).
narrative_ontology:cs_axiom_status(treaties_are_living_documents_subject_to_international_law, holdable).
narrative_ontology:cs_axiom_grounding('d995b80c-e4fb-4e0e-bb0a-d08bf0156217', treaties_are_living_documents_subject_to_international_law, conventional).
narrative_ontology:cs_reference_frame('d995b80c-e4fb-4e0e-bb0a-d08bf0156217', international_law_of_nations_framework).
narrative_ontology:cs_drift_state('d995b80c-e4fb-4e0e-bb0a-d08bf0156217', contemporary_reconciliation_era, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('d995b80c-e4fb-4e0e-bb0a-d08bf0156217', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, settler_state_international_reputation).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, settler_state_unilateral_resource_developers).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__nation_to_nation_reading, international_law_supremacy).
narrative_ontology:constraint_vindicates(historical_treaty_substrate__nation_to_nation_reading, indigenous_sovereignty_doctrine).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As co-equal sovereigns, Indigenous nations benefit from the recognition of their inherent rights and the requirement for their ongoing consent for territorial changes. Their exit options are constrained by historical power imbalances but strengthened by international legal frameworks.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations, beneficiary,
    organized, generational, constrained, regional).

% The settler state is bound by international treaty obligations, requiring it to engage in good-faith negotiations and obtain consent. This constrains its unilateral actions but enhances its international legitimacy and reputation.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_government, agenda_setter,
    institutional, generational, constrained, national).

% These developers bear the costs of requiring Indigenous consent and adhering to treaty principles, which can delay or prevent resource extraction projects. Their previous ability to operate unilaterally is curtailed.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_unilateral_resource_developers, payer,
    powerful, biographical, constrained, local).

% These bodies monitor compliance with international law and human rights, providing a forum for dispute resolution and influencing state behavior through reputational and legal pressure.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, international_legal_bodies, observer,
    institutional, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for ongoing, respectful relations between Indigenous nations and the settler state, ensuring mutual recognition of sovereignty and consent-based decision-making regarding shared territories.
% TRANSFER_FUNCTION: Transfers decision-making power and resource control from the settler state to Indigenous nations, requiring the settler state to share or cede authority over land and resources, and potentially transferring financial benefits to Indigenous communities.
% ABSENT_VOICES: Historical colonial administrators and proponents of 'terra nullius' would object, as their worldview denies Indigenous sovereignty and treats land as unowned prior to European settlement. Their voices are largely absent from modern international legal discourse but persist in some domestic legal interpretations.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal and political landscape would fundamentally shift. Indigenous nations would lose a key legal basis for their claims, leading to increased conflict and unilateral actions by the settler state. International relations would also be impacted, as the settler state would face reputational damage and potential legal challenges.
% FOUNDING_PROBLEM: The historical problem was the violent dispossession of Indigenous peoples, the denial of their sovereignty, and the imposition of colonial legal systems that failed to recognize pre-existing Indigenous rights and governance structures.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous legal scholars, international human rights organizations, and a growing body of comparative constitutional law attest that the problem of unresolved historical injustices and ongoing colonial practices remains live. This corroboration comes from outside the immediate settler state beneficiaries.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__nation_to_nation_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__nation_to_nation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__nation_to_nation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(historical_treaty_substrate__nation_to_nation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(historical_treaty_substrate__nation_to_nation_reading, 0.25, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(historical_treaty_substrate__nation_to_nation_reading_tests).
:- end_tests(historical_treaty_substrate__nation_to_nation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness (0.25) is relatively low from the perspective of Indigenous nations, as this reading grants them significant rights and protections, shifting power away from the settler state. Suppression (0.40) is moderate, reflecting the ongoing struggle to fully implement these principles against historical inertia and resistance from settler institutions. The theater ratio (0.10) is low, as the principles of this reading are genuinely pursued by many actors, though often imperfectly. Resistance (0.70) is high, indicating active advocacy and legal challenges by Indigenous nations to uphold this interpretation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Indigenous nations, this reading is a crucial mechanism for justice and self-determination, reducing historical extraction. From the perspective of settler state resource developers, it imposes significant costs and constraints on their operations, increasing their 'extraction' by requiring consent and benefit-sharing. The settler state government itself experiences a mixed picture: constrained in some areas, but gaining international legitimacy.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations are primary beneficiaries (d near 0.0) as this reading empowers them. The settler state government is also a beneficiary (d near 0.2) due to enhanced international reputation and reduced conflict, despite some constraints. Unilateral resource developers are clear targets (d near 1.0) as their previous extractive practices are curtailed. International legal bodies act as analytical observers.
 *
 * MANDATROPHY ANALYSIS:
 *   This classification prevents mislabeling genuine efforts towards decolonization and reconciliation as mere extraction. While it constrains the settler state, it does so by re-establishing a more equitable coordination framework based on mutual recognition, rather than imposing a new form of extraction. The 'live' status of the founding problem (historical dispossession) indicates that the mandate for this reading is still highly relevant and necessary.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    implementation_gap_vs_legal_principle,
    'Is the ''nation-to-nation'' principle genuinely implemented in practice, or does an implementation gap persist due to settler state resistance?',
    'Empirical analysis of consent processes, resource revenue sharing, and legal outcomes in treaty disputes. If practice consistently falls short of the principle, the effective extractiveness for Indigenous nations is higher than this reading suggests.',
    'If a significant implementation gap exists, the constraint''s effective extractiveness for Indigenous nations is higher, and its classification might shift towards a ''Tangled Rope'' or ''Snare'' from their perspective, indicating a gap between claimed principle and actual operation.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(implementation_gap_vs_legal_principle, empirical, 'Gap between the legal principle of nation-to-nation relations and its practical implementation.').

omega_variable(
    sovereignty_definition_ambiguity,
    'How is ''sovereignty'' defined in the context of historical treaties, and does this definition align between Indigenous nations and the settler state?',
    'Comparative legal analysis of Indigenous legal traditions and settler state constitutional law, alongside ethnographic research into Indigenous governance. Divergent definitions could lead to ongoing disputes despite agreement on ''nation-to-nation'' language.',
    'If definitions of sovereignty remain fundamentally misaligned, the coordination function of this reading is undermined, increasing friction and potentially leading to higher effective extraction for Indigenous nations due to persistent power imbalances in interpretation.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(sovereignty_definition_ambiguity, conceptual, 'Ambiguity in the definition of ''sovereignty'' between parties to historical treaties.').

omega_variable(
    kernel_reading_extinguishment_vs_nation_to_nation,
    'Is this ''nation-to-nation'' reading logically compatible with the ''extinguishment'' reading, or does it fundamentally foreclose it?',
    'Conceptual analysis of the core premises: if ''extinguishment'' asserts a one-time cession of all sovereignty and ''nation-to-nation'' asserts ongoing, inherent sovereignty, they are logically contradictory within a single legal framework.',
    'If ''forecloses'', then the persistence of the ''extinguishment'' reading indicates a deep, unresolved conflict at the kernel level, leading to higher suppression and extractiveness for Indigenous nations where the extinguishment reading is applied.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(kernel_reading_extinguishment_vs_nation_to_nation, conceptual, 'Relationship between the ''nation-to-nation'' and ''extinguishment'' readings of historical treaties.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__nation_to_nation_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t1945, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1945, 0.05).
narrative_ontology:measurement(hist_tr_t1965, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1965, 0.08).
narrative_ontology:measurement(hist_tr_t1985, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1985, 0.1).
narrative_ontology:measurement(hist_tr_t2005, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2005, 0.1).
narrative_ontology:measurement(hist_tr_t2024, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(hist_be_t1945, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1945, 0.9).
narrative_ontology:measurement(hist_be_t1965, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1965, 0.75).
narrative_ontology:measurement(hist_be_t1985, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1985, 0.5).
narrative_ontology:measurement(hist_be_t2005, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2005, 0.35).
narrative_ontology:measurement(hist_be_t2024, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2024, 0.25).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t1945, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1945, 0.95).
narrative_ontology:measurement(hist_su_t1965, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1965, 0.8).
narrative_ontology:measurement(hist_su_t1985, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(hist_su_t2005, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2005, 0.45).
narrative_ontology:measurement(hist_su_t2024, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2024, 0.4).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__nation_to_nation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__stewardship_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'historical_treaty_substrate' kernel. This 'nation-to-nation' reading emphasizes Indigenous sovereignty and ongoing consent, contrasting with the 'extinguishment' reading (property transaction) and the 'stewardship' reading (relational pact).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
