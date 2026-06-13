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
 *   constraint_id: historical_treaty_substrate__nation_to_nation_reading
 *   human_readable: Historical Treaty Substrate (Nation-to-Nation Reading)
 *   domain: legal_anthropology/indigenous_law/comparative_constitutional_theory
 *
 * SUMMARY:
 *   This constraint represents the 'nation-to-nation' reading of historical
 *   treaties, where they are understood as ongoing international agreements
 *   between sovereign equals, requiring continuous consent and subject to
 *   modern international law. This reading contrasts sharply with colonial
 *   interpretations that view treaties as one-time land cessions. The metrics
 *   reflect a historical shift from high extraction and suppression (when
 *   this reading was suppressed) towards a more balanced, though still
 *   contested, coordination function.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(historical_treaty_substrate__nation_to_nation_reading, 0.4).
domain_priors:suppression_score(historical_treaty_substrate__nation_to_nation_reading, 0.3).
domain_priors:theater_ratio(historical_treaty_substrate__nation_to_nation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, extractiveness, 0.4).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 0.3).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, accessibility_collapse, 0.2).
narrative_ontology:constraint_metric(historical_treaty_substrate__nation_to_nation_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(historical_treaty_substrate__nation_to_nation_reading, rope).
narrative_ontology:human_readable(historical_treaty_substrate__nation_to_nation_reading, "Historical Treaty Substrate (Nation-to-Nation Reading)").
narrative_ontology:topic_domain(historical_treaty_substrate__nation_to_nation_reading, "legal_anthropology/indigenous_law/comparative_constitutional_theory").

domain_priors:requires_active_enforcement(historical_treaty_substrate__nation_to_nation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(historical_treaty_substrate__nation_to_nation_reading, '4f91ba8a-939e-4283-8496-6168af297ad5').
narrative_ontology:cs_kernel_codification('4f91ba8a-939e-4283-8496-6168af297ad5', fixed_text).
narrative_ontology:cs_authority_grounding('4f91ba8a-939e-4283-8496-6168af297ad5', lineage).
narrative_ontology:cs_interpretation_layer_present('4f91ba8a-939e-4283-8496-6168af297ad5').
narrative_ontology:cs_reading_relation('4f91ba8a-939e-4283-8496-6168af297ad5', historical_treaty_substrate__extinguishment_reading, forecloses).
narrative_ontology:cs_reading_relation('4f91ba8a-939e-4283-8496-6168af297ad5', historical_treaty_substrate__stewardship_reading, coexists_with).
narrative_ontology:cs_axiom('4f91ba8a-939e-4283-8496-6168af297ad5', foundational, indigenous_nations_retain_inherent_sovereignty).
narrative_ontology:cs_axiom_status(indigenous_nations_retain_inherent_sovereignty, holdable).
narrative_ontology:cs_axiom_grounding('4f91ba8a-939e-4283-8496-6168af297ad5', indigenous_nations_retain_inherent_sovereignty, deontological).
narrative_ontology:cs_axiom('4f91ba8a-939e-4283-8496-6168af297ad5', foundational, treaties_are_living_documents_subject_to_international_law).
narrative_ontology:cs_axiom_status(treaties_are_living_documents_subject_to_international_law, holdable).
narrative_ontology:cs_axiom_grounding('4f91ba8a-939e-4283-8496-6168af297ad5', treaties_are_living_documents_subject_to_international_law, conventional).
narrative_ontology:cs_reference_frame('4f91ba8a-939e-4283-8496-6168af297ad5', post_un_declaration_on_indigenous_rights).
narrative_ontology:cs_drift_state('4f91ba8a-939e-4283-8496-6168af297ad5', contemporary_legal_challenges, gap(revival_pressure, substantial, true)).
narrative_ontology:cs_created_at('4f91ba8a-939e-4283-8496-6168af297ad5', '').
narrative_ontology:cs_kernel_id(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations).
narrative_ontology:constraint_beneficiary(historical_treaty_substrate__nation_to_nation_reading, settler_state_international_reputation).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, settler_state_unilateral_resource_extraction).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_corporations).
narrative_ontology:constraint_victim(historical_treaty_substrate__nation_to_nation_reading, settler_population).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As co-equal sovereigns, Indigenous nations benefit from the recognition of their inherent rights and the requirement for their free, prior, and informed consent on territorial matters. Their exit options are constrained by historical power imbalances but strengthened by international law.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, indigenous_nations, beneficiary,
    organized, generational, constrained, regional).

% The settler state is bound by international treaty law, requiring ongoing consent and negotiation with Indigenous nations. This constrains its ability for unilateral resource extraction but enhances its international reputation for upholding human rights and rule of law.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_state_government, agenda_setter,
    institutional, generational, constrained, national).

% These corporations face increased legal and political hurdles, including the need for Indigenous consent and revenue sharing, which raises their operating costs and introduces uncertainty for projects on traditional territories. Their ability to operate unilaterally is curtailed.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, resource_extraction_corporations, payer,
    powerful, biographical, constrained, global).

% These bodies (e.g., UN, ICJ) monitor compliance with international human rights and treaty law, providing a framework that supports the nation-to-nation reading and can exert pressure on settler states to adhere to their obligations.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, international_legal_bodies, observer,
    institutional, civilizational, analytical, global).

% The settler population, through its government, bears the costs of renegotiating treaties, potential revenue sharing, and legal challenges, but also benefits from a more just and stable society and improved international standing.
narrative_ontology:constraint_stakeholder(historical_treaty_substrate__nation_to_nation_reading, settler_population, payer,
    organized, generational, mobile, national).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a framework for ongoing, consensual relations between Indigenous nations and settler states regarding land, resources, and governance, preventing unilateral action and fostering mutual respect under international law.
% TRANSFER_FUNCTION: Transfers decision-making power and resource benefits from the settler state to Indigenous nations, while transferring legitimacy and international standing to the settler state.
% ABSENT_VOICES: Historical colonial administrators and legal theorists who framed treaties as land cessions would object, as their interpretations are directly challenged by this reading. They are absent from contemporary legal and political discourse that increasingly recognizes Indigenous sovereignty.
% DISAPPEARANCE_RATIONALE: If this reading vanished, the legal and political landscape would revert to a more colonial interpretation, leading to increased conflict over land and resources, erosion of Indigenous rights, and damage to the settler state's international reputation. Existing legal frameworks would be undermined.
% FOUNDING_PROBLEM: The historical problem was the need to establish peaceful coexistence and define relationships between distinct sovereign entities occupying shared territories, often through formal agreements.
% FOUNDING_PROBLEM_CORROBORATION: Indigenous nations consistently attest that the problem of defining and upholding nation-to-nation relationships is live and ongoing. International legal scholars and human rights organizations corroborate this, emphasizing the persistent need for decolonization and recognition of Indigenous rights, from outside the settler state's immediate beneficiaries.
narrative_ontology:disappearance_verdict(historical_treaty_substrate__nation_to_nation_reading, world_rearranges).
narrative_ontology:founding_problem_status(historical_treaty_substrate__nation_to_nation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(historical_treaty_substrate__nation_to_nation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(historical_treaty_substrate__nation_to_nation_reading, 'none', 1).

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
 *   The extractiveness (0.4) and suppression (0.3) are relatively low for this reading, as it aims to reduce unilateral extraction and coercion. The theater ratio (0.1) is low because the reading emphasizes genuine engagement and consent over performative gestures. Resistance (0.7) is high, reflecting the ongoing struggle by Indigenous nations to assert this interpretation against historical and ongoing colonial practices. Accessibility collapse (0.2) is low, as this reading actively seeks to open alternatives to colonial governance.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of Indigenous nations, this reading is a genuine Rope, facilitating coordination and justice. From the perspective of settler state entities accustomed to unilateral power, it is a Tangled Rope or even a Snare, as it imposes significant constraints and costs on their historical practices. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   Indigenous nations are beneficiaries, gaining recognition of sovereignty and consent rights. The settler state government is also a beneficiary in terms of international legitimacy, but a payer in terms of constrained unilateral action. Resource extraction corporations are clear payers, facing increased obligations. The settler population is a payer through government obligations but benefits from a more just society. International legal bodies act as observers, reinforcing the framework.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    legal_recognition_vs_practice,
    'To what extent is the ''nation-to-nation'' reading genuinely implemented in practice by settler states, versus merely acknowledged in legal theory or rhetoric?',
    'Empirical analysis of resource development projects on Indigenous lands: frequency of free, prior, and informed consent (FPIC) processes, outcomes of negotiations, and enforcement of Indigenous veto rights.',
    'If implementation lags significantly behind rhetoric, the effective extractiveness and suppression of the constraint are higher than measured, indicating a ''tangled_rope'' or ''snare'' in practice, despite the ''rope'' claim.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(legal_recognition_vs_practice, empirical, 'Gap between legal theory and practical implementation of nation-to-nation treaty principles.').

omega_variable(
    sovereignty_definition_ambiguity,
    'Is the concept of ''co-equal sovereignty'' as applied to Indigenous nations and settler states truly symmetrical, or does it implicitly retain elements of settler state paramountcy?',
    'Comparative legal analysis of judicial decisions and legislative actions in cases where Indigenous and settler state jurisdictions conflict, particularly regarding resource management and self-governance.',
    'If implicit paramountcy persists, Indigenous nations'' exit options are more ''constrained'' or ''identity_locked'' than ''mobile'', increasing their effective extraction and shifting the constraint towards a ''tangled_rope'' from their seat.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(sovereignty_definition_ambiguity, conceptual, 'Ambiguity in the practical symmetry of co-equal sovereignty.').

omega_variable(
    mandate_for_reconciliation,
    'Is the settler state''s engagement with the ''nation-to-nation'' reading driven by a genuine commitment to reconciliation and justice, or primarily by external pressures (e.g., international reputation, legal challenges)?',
    'Analysis of policy drivers, internal government documents, and public discourse over time. Assess whether engagement persists in the absence of external pressure.',
    'If driven primarily by external pressure, the ''beneficiary'' status of the settler state''s international reputation is more pronounced, and the constraint''s stability is more contingent on maintaining those external pressures, potentially revealing a ''piton'' if the external pressure wanes.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(mandate_for_reconciliation, preference, 'Motivation for settler state''s adoption of nation-to-nation reading.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(historical_treaty_substrate__nation_to_nation_reading, 1945, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(hist_tr_t1945, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1945, 0.5).
narrative_ontology:measurement(hist_tr_t1965, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1965, 0.4).
narrative_ontology:measurement(hist_tr_t1985, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 1985, 0.3).
narrative_ontology:measurement(hist_tr_t2005, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2005, 0.2).
narrative_ontology:measurement(hist_tr_t2024, historical_treaty_substrate__nation_to_nation_reading, theater_ratio, 2024, 0.1).

% Extraction over time
narrative_ontology:measurement(hist_be_t1945, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1945, 0.8).
narrative_ontology:measurement(hist_be_t1965, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1965, 0.7).
narrative_ontology:measurement(hist_be_t1985, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 1985, 0.6).
narrative_ontology:measurement(hist_be_t2005, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2005, 0.5).
narrative_ontology:measurement(hist_be_t2024, historical_treaty_substrate__nation_to_nation_reading, base_extractiveness, 2024, 0.4).

% Suppression requirement over time
narrative_ontology:measurement(hist_su_t1945, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1945, 0.9).
narrative_ontology:measurement(hist_su_t1965, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1965, 0.8).
narrative_ontology:measurement(hist_su_t1985, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 1985, 0.6).
narrative_ontology:measurement(hist_su_t2005, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2005, 0.4).
narrative_ontology:measurement(hist_su_t2024, historical_treaty_substrate__nation_to_nation_reading, suppression_requirement, 2024, 0.3).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(historical_treaty_substrate__nation_to_nation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__extinguishment_reading).
narrative_ontology:affects_constraint(historical_treaty_substrate__nation_to_nation_reading, historical_treaty_substrate__stewardship_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'historical_treaty_substrate' kernel. This 'nation-to-nation' reading emphasizes treaties as international agreements between sovereign equals, contrasting with the 'extinguishment' (land cession) and 'stewardship' (relational pact) readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
