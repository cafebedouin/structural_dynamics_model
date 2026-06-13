% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__partial_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__partial_withdrawal_reading, []).

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
    narrative_ontology:measurement_basis/2,
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
 *   constraint_id: unsc_242_withdrawal_clause__partial_withdrawal_reading
 *   human_readable: UNSC Resolution 242 Withdrawal Clause (Partial Withdrawal Reading)
 *   domain: international_law/diplomatic_history/treaty_interpretation
 *
 * SUMMARY:
 *   This constraint represents the 'partial withdrawal' reading of UNSC
 *   Resolution 242, which interprets the English text's indefinite article
 *   ('withdrawal of Israeli armed forces from territories occupied in the
 *   recent conflict') as permitting retention of some strategic territories
 *   for secure boundaries. This reading converts textual ambiguity into
 *   negotiating leverage for the occupying power and mediating states, while
 *   imposing costs on claimant states and displaced populations. It is a
 *   Tangled Rope because it offers a framework for coordination (peace
 *   negotiations) but with significant asymmetric extraction due to the
 *   interpretive flexibility it grants one party.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.55).
domain_priors:suppression_score(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.6).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.2).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, extractiveness, 0.55).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0.6).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0.2).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, accessibility_collapse, 0.4).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__partial_withdrawal_reading, "UNSC Resolution 242 Withdrawal Clause (Partial Withdrawal Reading)").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__partial_withdrawal_reading, "international_law/diplomatic_history/treaty_interpretation").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__partial_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'fbe64fa2-2263-4492-ae11-0b8d9f564971').
narrative_ontology:cs_kernel_codification('fbe64fa2-2263-4492-ae11-0b8d9f564971', fixed_text).
narrative_ontology:cs_authority_grounding('fbe64fa2-2263-4492-ae11-0b8d9f564971', lineage).
narrative_ontology:cs_interpretation_layer_present('fbe64fa2-2263-4492-ae11-0b8d9f564971').
narrative_ontology:cs_reading_relation('fbe64fa2-2263-4492-ae11-0b8d9f564971', unsc_242_withdrawal_clause__maximal_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('fbe64fa2-2263-4492-ae11-0b8d9f564971', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('fbe64fa2-2263-4492-ae11-0b8d9f564971', foundational, indefinite_article_permits_discretion).
narrative_ontology:cs_axiom_status(indefinite_article_permits_discretion, holdable).
narrative_ontology:cs_axiom_grounding('fbe64fa2-2263-4492-ae11-0b8d9f564971', indefinite_article_permits_discretion, conventional).
narrative_ontology:cs_axiom('fbe64fa2-2263-4492-ae11-0b8d9f564971', foundational, secure_boundaries_require_territorial_adjustments).
narrative_ontology:cs_axiom_status(secure_boundaries_require_territorial_adjustments, holdable).
narrative_ontology:cs_axiom_grounding('fbe64fa2-2263-4492-ae11-0b8d9f564971', secure_boundaries_require_territorial_adjustments, instrumental).
narrative_ontology:cs_reference_frame('fbe64fa2-2263-4492-ae11-0b8d9f564971', negotiated_peace_with_security_guarantees).
narrative_ontology:cs_drift_state('fbe64fa2-2263-4492-ae11-0b8d9f564971', contemporary_diplomatic_stalemate, gap(practice_drift, substantial, false)).
narrative_ontology:cs_created_at('fbe64fa2-2263-4492-ae11-0b8d9f564971', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_states).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, claimant_states).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, displaced_populations).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the ambiguity, using it to justify retention of strategically important territories and to negotiate phased withdrawals. Actively interprets the 'indefinite article' in English text as permitting partial withdrawal. Its continued presence in disputed territories is enabled by this reading.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power, agenda_setter,
    institutional, generational, constrained, regional).

% Bear the cost of continued occupation and territorial loss. They interpret the resolution as requiring full withdrawal from all occupied territories and are disadvantaged by the ambiguity. Their diplomatic and legal efforts are continuously undermined by the 'partial withdrawal' interpretation.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, claimant_states, payer,
    organized, generational, trapped, regional).

% Benefit from the flexibility this reading provides for diplomatic engagement and phased peace processes. The ambiguity allows them to broker agreements without demanding immediate, full withdrawal, maintaining their role as indispensable intermediaries.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_states, beneficiary,
    institutional, biographical, mobile, global).

% Are the ultimate victims, unable to return to their homes due to continued occupation. Their claims for return are deferred or denied by the 'partial withdrawal' interpretation, and their identity is often tied to the right of return.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, displaced_populations, payer,
    powerless, generational, identity_locked, local).

% Analyze the legal implications of the textual ambiguity and the historical intent of the drafters. They provide academic commentary that influences, but does not directly control, the diplomatic and political interpretations.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, international_legal_scholars, observer,
    analytical, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Provides a framework for negotiating territorial disputes and achieving a 'just and lasting peace' by allowing for phased withdrawal and security arrangements, rather than an immediate, unconditional return to pre-conflict lines.
% TRANSFER_FUNCTION: Transfers negotiating leverage and de facto control over strategic territories from claimant states to the occupying power, mediated by international diplomatic efforts. It also transfers the burden of displacement and insecurity to affected populations.
% ABSENT_VOICES: The populations directly affected by occupation and displacement, whose right to self-determination and return is often sidelined in diplomatic negotiations that prioritize state-level security concerns. Their voices are often represented by claimant states, but their direct agency is limited.
% DISAPPEARANCE_RATIONALE: If this specific reading of UNSC 242 vanished, the diplomatic landscape would shift dramatically. The occupying power would lose a key legal justification for retaining territories, increasing pressure for full withdrawal. Claimant states would gain leverage, and mediating states would need a new framework for negotiation. The status quo of occupation would become legally untenable, forcing a rearrangement of regional power dynamics.
% FOUNDING_PROBLEM: The resolution was drafted in the aftermath of the 1967 Six-Day War to establish principles for a comprehensive peace settlement, addressing both the 'inadmissibility of the acquisition of territory by war' and the need for 'secure and recognized boundaries' for all states in the region.
% FOUNDING_PROBLEM_CORROBORATION: The problem remains live as the core territorial disputes and security concerns persist. International mediators, UN bodies, and the parties themselves (albeit with differing interpretations) continue to refer to UNSC 242 as the foundational document for peace efforts. The ongoing conflict and diplomatic stalemates corroborate the persistence of the underlying problem.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__partial_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__partial_withdrawal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'none', 1).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__partial_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__partial_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   Extractiveness is moderate (0.55) because the ambiguity allows the occupying power to retain territory, but it is not absolute, as international pressure for withdrawal remains. Suppression is moderate (0.6) as the occupying power actively enforces its interpretation through diplomatic and military means, limiting the options of claimant states. Theater ratio is low (0.2) because the diplomatic efforts and security justifications are genuinely pursued, even if the underlying interpretation is self-serving. The constraint's persistence relies on active enforcement of this interpretation and the suppression of alternative readings.
 *
 * PERSPECTIVAL GAP:
 *   The occupying power perceives this reading as a legitimate interpretation necessary for its security, enabling coordination towards peace. Claimant states perceive it as an extractive mechanism that legitimizes occupation and undermines international law. The engine's per-seat classification will reflect this divergence, with the occupying power's seat computing as a Rope or Scaffold, and the claimant states' seats computing as a Snare or Tangled Rope.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying power and mediating states are beneficiaries, as this reading provides them with diplomatic flexibility and leverage. Claimant states and displaced populations are victims, bearing the costs of continued occupation and deferred resolution. The directionality for the occupying power is low (beneficiary), while for claimant states and displaced populations it is high (target). Mediating states benefit from their role as brokers, enabled by the ambiguity.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_resolution,
    'Is the textual ambiguity (indefinite ''territories'' vs. definite ''les territoires'') a genuine linguistic difference or a deliberate diplomatic compromise intended to be resolved through negotiation?',
    'Analysis of drafting records and diplomatic correspondence from 1967, and expert testimony from the drafters themselves (if available and consistent).',
    'If deliberate compromise, the ambiguity is a feature, not a bug, and the ''partial withdrawal'' reading is a valid interpretive strategy. If genuine linguistic difference, one language version might be deemed authoritative, potentially foreclosing the ''partial withdrawal'' reading.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(textual_ambiguity_resolution, empirical, 'Origin of the textual ambiguity in UNSC 242.').

omega_variable(
    secure_boundaries_principle_scope,
    'Does the ''secure and recognized boundaries'' principle in UNSC 242 inherently permit the retention of occupied territory, or does it primarily refer to the nature of future negotiated borders?',
    'Legal analysis of customary international law regarding territorial acquisition by force, and historical precedents in UN resolutions concerning post-conflict boundaries.',
    'If it permits retention, the ''partial withdrawal'' reading gains stronger legal grounding. If it refers only to future negotiated borders, the ''partial withdrawal'' reading is weakened, increasing its extractiveness.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(secure_boundaries_principle_scope, conceptual, 'Scope of the ''secure boundaries'' principle in UNSC 242.').

omega_variable(
    mandatrophy_of_negotiation,
    'Has the ''partial withdrawal'' reading, originally intended to facilitate negotiation, become a permanent justification for occupation, thereby outliving its coordination mandate?',
    'Longitudinal study of diplomatic efforts: if negotiations consistently fail to produce full withdrawal over decades, and the ''partial withdrawal'' reading is cited as justification for the stalemate, it indicates mandatrophy.',
    'If mandatrophy is confirmed, the constraint shifts from a Tangled Rope (coordination + extraction) to a Snare (pure extraction), as its coordination function has atrophied, leaving only the extractive component.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(mandatrophy_of_negotiation, empirical, 'Whether the partial withdrawal reading has become a permanent justification for occupation.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__partial_withdrawal_reading, 1967, 2024).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement_basis(unsc_tr_t1967, observed).
narrative_ontology:measurement(unsc_tr_t1973, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1973, 0.15).
narrative_ontology:measurement_basis(unsc_tr_t1973, observed).
narrative_ontology:measurement(unsc_tr_t1987, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1987, 0.2).
narrative_ontology:measurement_basis(unsc_tr_t1987, observed).
narrative_ontology:measurement(unsc_tr_t1993, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 1993, 0.18).
narrative_ontology:measurement_basis(unsc_tr_t1993, observed).
narrative_ontology:measurement(unsc_tr_t2000, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2000, 0.22).
narrative_ontology:measurement_basis(unsc_tr_t2000, observed).
narrative_ontology:measurement(unsc_tr_t2024, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 2024, 0.2).
narrative_ontology:measurement_basis(unsc_tr_t2024, observed).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1967, 0.45).
narrative_ontology:measurement_basis(unsc_be_t1967, observed).
narrative_ontology:measurement(unsc_be_t1973, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1973, 0.5).
narrative_ontology:measurement_basis(unsc_be_t1973, observed).
narrative_ontology:measurement(unsc_be_t1987, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1987, 0.55).
narrative_ontology:measurement_basis(unsc_be_t1987, observed).
narrative_ontology:measurement(unsc_be_t1993, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 1993, 0.52).
narrative_ontology:measurement_basis(unsc_be_t1993, observed).
narrative_ontology:measurement(unsc_be_t2000, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2000, 0.58).
narrative_ontology:measurement_basis(unsc_be_t2000, observed).
narrative_ontology:measurement(unsc_be_t2024, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 2024, 0.55).
narrative_ontology:measurement_basis(unsc_be_t2024, observed).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1967, 0.5).
narrative_ontology:measurement_basis(unsc_su_t1967, observed).
narrative_ontology:measurement(unsc_su_t1973, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1973, 0.55).
narrative_ontology:measurement_basis(unsc_su_t1973, observed).
narrative_ontology:measurement(unsc_su_t1987, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1987, 0.6).
narrative_ontology:measurement_basis(unsc_su_t1987, observed).
narrative_ontology:measurement(unsc_su_t1993, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 1993, 0.58).
narrative_ontology:measurement_basis(unsc_su_t1993, observed).
narrative_ontology:measurement(unsc_su_t2000, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2000, 0.62).
narrative_ontology:measurement_basis(unsc_su_t2000, observed).
narrative_ontology:measurement(unsc_su_t2024, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 2024, 0.6).
narrative_ontology:measurement_basis(unsc_su_t2024, observed).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__partial_withdrawal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__maximal_withdrawal_reading).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause__interpretive_authority_structure).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the UNSC Resolution 242 withdrawal clause kernel. This 'partial withdrawal' reading emphasizes the indefinite article in the English text, allowing for discretionary withdrawal. It is linked to the 'maximal withdrawal' reading (requiring full withdrawal) and the 'interpretive authority structure' (contesting who interprets the resolution).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
