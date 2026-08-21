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
 *   constraint_id: unsc_242_withdrawal_clause__partial_withdrawal_reading
 *   human_readable: UNSC Resolution 242 Withdrawal Clause: Partial Withdrawal Reading
 *   domain: international_law/diplomatic_history
 *
 * SUMMARY:
 *   This constraint represents the 'partial withdrawal' reading of UNSC
 *   Resolution 242, which interprets the English indefinite article
 *   'territories' as granting discretion to the occupying power to retain
 *   strategic territories for secure boundaries. This reading converts
 *   textual ambiguity into negotiating leverage, benefiting the occupying
 *   power and mediating states, while imposing costs on claimant states. It
 *   is a specific interpretation within a broader kernel of contested
 *   meanings of Resolution 242.
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
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__partial_withdrawal_reading, resistance, 0.5).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__partial_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__partial_withdrawal_reading, "UNSC Resolution 242 Withdrawal Clause: Partial Withdrawal Reading").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__partial_withdrawal_reading, "international_law/diplomatic_history").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__partial_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__partial_withdrawal_reading, '7397a190-8967-4fde-a44c-75f59c5c01fb').
narrative_ontology:cs_kernel_codification('7397a190-8967-4fde-a44c-75f59c5c01fb', fixed_text).
narrative_ontology:cs_authority_grounding('7397a190-8967-4fde-a44c-75f59c5c01fb', lineage).
narrative_ontology:cs_interpretation_layer_present('7397a190-8967-4fde-a44c-75f59c5c01fb').
narrative_ontology:cs_reading_relation('7397a190-8967-4fde-a44c-75f59c5c01fb', unsc_242_withdrawal_clause__maximal_withdrawal_reading, coexists_with).
narrative_ontology:cs_reading_relation('7397a190-8967-4fde-a44c-75f59c5c01fb', unsc_242_withdrawal_clause__interpretive_authority_structure, influences).
narrative_ontology:cs_axiom('7397a190-8967-4fde-a44c-75f59c5c01fb', foundational, indefinite_article_grants_discretion).
narrative_ontology:cs_axiom_status(indefinite_article_grants_discretion, holdable).
narrative_ontology:cs_axiom_grounding('7397a190-8967-4fde-a44c-75f59c5c01fb', indefinite_article_grants_discretion, conventional).
narrative_ontology:cs_axiom('7397a190-8967-4fde-a44c-75f59c5c01fb', foundational, security_needs_permit_territorial_retention).
narrative_ontology:cs_axiom_status(security_needs_permit_territorial_retention, holdable).
narrative_ontology:cs_axiom_grounding('7397a190-8967-4fde-a44c-75f59c5c01fb', security_needs_permit_territorial_retention, instrumental).
narrative_ontology:cs_reference_frame('7397a190-8967-4fde-a44c-75f59c5c01fb', post_1967_diplomatic_framework).
narrative_ontology:cs_drift_state('7397a190-8967-4fde-a44c-75f59c5c01fb', contemporary_stalemate, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('7397a190-8967-4fde-a44c-75f59c5c01fb', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__partial_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_states).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__partial_withdrawal_reading, claimant_states).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Interprets UNSC 242's withdrawal clause as permitting retention of strategic territories for security, leveraging the indefinite article in the English text. Benefits from negotiating leverage and de facto control over occupied lands, while facing international pressure for withdrawal.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power, agenda_setter,
    institutional, generational, constrained, regional).

% Seek full withdrawal from all occupied territories, viewing the 'partial withdrawal' reading as a violation of international law and territorial integrity. Bear the costs of continued occupation and the diplomatic stalemate, with limited direct enforcement options.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, claimant_states, payer,
    institutional, generational, constrained, regional).

% Engage in diplomatic efforts to facilitate peace negotiations based on UNSC 242. Benefit from their role as brokers and the stability (or managed instability) that the ongoing diplomatic process provides, even if progress is slow.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, mediating_states, beneficiary,
    institutional, biographical, mobile, global).

% Populations displaced from occupied territories who have no direct voice in the formal diplomatic interpretation or negotiation process. Their claims for return and restitution are often secondary to state-level security and territorial negotiations.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, displaced_populations, excluded,
    powerless, generational, trapped, local).

% Analyze the legal implications of different interpretations of UNSC 242, contributing to academic discourse and informing policy debates, but without direct power to enforce or alter the constraint.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__partial_withdrawal_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__partial_withdrawal_reading, occupying_power).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__partial_withdrawal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To establish a diplomatic framework for peace negotiations in the Middle East following the 1967 conflict, aiming to balance security concerns of states with territorial claims.
% TRANSFER_FUNCTION: Transfers negotiating leverage and de facto control over strategic territories to the occupying power, in exchange for a commitment to eventual withdrawal from *some* territories and a framework for peace.
% ABSENT_VOICES: Populations displaced from occupied territories and non-state actors involved in the conflict are largely excluded from the formal diplomatic process; they would advocate for full and immediate withdrawal and self-determination.
% DISAPPEARANCE_RATIONALE: If the 'partial withdrawal' reading and its associated diplomatic framework vanished, the basis for decades of peace negotiations would collapse, likely leading to renewed conflict, increased instability, and a diplomatic vacuum in the region.
% FOUNDING_PROBLEM: The aftermath of the 1967 Six-Day War, characterized by Israeli occupation of Arab territories, a lack of secure and recognized borders, and an urgent need for a diplomatic path to peace and stability in the Middle East.
% FOUNDING_PROBLEM_CORROBORATION: The persistence of the Israeli-Palestinian conflict, ongoing UN resolutions, and continuous international diplomatic efforts corroborate that the core problems of secure borders and territorial claims remain live, though the efficacy and interpretation of UNSC 242 as a solution are highly contested by various parties and international legal bodies.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__partial_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__partial_withdrawal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__partial_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__partial_withdrawal_reading, 0.55, 'gemini-2.5-flash', 'none', direct).

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
 *   The extractiveness is moderate (0.55 at start, rising to 0.65) because this reading permits the retention of territory, which is a significant extraction from claimant states, but it also mandates *some* withdrawal and a framework for peace. Suppression is moderate (0.6, rising to 0.7) as it relies on diplomatic pressure, military realities, and the power imbalance to maintain the status quo. The theater ratio is low (0.2, stable) because the diplomatic process, while slow and contested, is genuinely active, and the interpretation serves a real, if asymmetric, function in ongoing negotiations. The claimed type is Tangled Rope due to its dual function of coordinating a peace process while enabling asymmetric extraction.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the occupying power, this reading provides necessary flexibility for national security and a basis for phased, negotiated peace. From the perspective of claimant states, it is a pretext for prolonged occupation and a violation of territorial integrity. Mediating states view it as a pragmatic basis for ongoing diplomacy. The engine will compute these divergent classifications from the structural data.
 *
 * DIRECTIONALITY LOGIC:
 *   The occupying power is a primary beneficiary (low d) as it gains negotiating leverage and de facto control over strategic territories. Mediating states are also beneficiaries (low d) as their role in the diplomatic process is sustained. Claimant states are victims (high d) as they bear the cost of non-withdrawal and the erosion of their territorial claims. Displaced populations are excluded (high d) as their voices are not directly represented in the formal diplomatic process.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    indefinite_article_intent,
    'Is the indefinite article ''territories'' in the English text of UNSC Resolution 242 truly intended by the drafters to grant discretionary withdrawal, or is it a linguistic artifact that has been leveraged for political ends?',
    'Historical analysis of drafting records, diplomatic correspondence, and statements by key drafters from the time of the resolution''s adoption.',
    'If a linguistic artifact, the partial withdrawal reading is a Snare, as its core justification is revealed as a pretext for extraction. If intended, it remains a Tangled Rope, reflecting a genuine (though asymmetric) coordination function.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(indefinite_article_intent, empirical, 'Ambiguity regarding the drafters'' intent behind the indefinite article in UNSC 242.').

omega_variable(
    maximal_withdrawal_reading_impact,
    'If the ''maximal withdrawal'' reading (withdrawal from all occupied territories) were universally adopted and enforced, would the ''partial withdrawal'' reading become untenable within a single international legal framework?',
    'A definitive ruling by an international judicial body (e.g., ICJ) or a new, unambiguous UN Security Council resolution that explicitly clarifies the scope of withdrawal.',
    'If the maximal reading forecloses the partial reading, this constraint would be overridden, leading to a reclassification (likely to Snare if the partial reading is deemed a pretext, or to Piton if it merely atrophies). If they can coexist, the contest continues.',
    confidence_without_resolution(low)
).

narrative_ontology:omega_variable(maximal_withdrawal_reading_impact, conceptual, 'The potential for the maximal withdrawal reading to logically foreclose the partial withdrawal reading.').

omega_variable(
    interpretive_authority_legitimacy,
    'Who holds the legitimate authority to definitively interpret the withdrawal clause of UNSC 242: the International Court of Justice, the original drafting states, or customary international practice as established by state behavior?',
    'A global consensus among states on the hierarchy of interpretive authorities for UN Security Council resolutions, or a specific mandate granted to a judicial body by all relevant parties.',
    'Resolution of interpretive authority would either solidify the legitimacy of the partial withdrawal reading (if its preferred authority is validated) or undermine it (if a competing authority with a different interpretation is validated).',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(interpretive_authority_legitimacy, preference, 'Contestation over the legitimate interpretive authority for UNSC 242''s withdrawal clause.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__partial_withdrawal_reading, 1967, 2023).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 0, 0.15).
narrative_ontology:measurement(unsc_tr_t10, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 10, 0.18).
narrative_ontology:measurement(unsc_tr_t20, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 20, 0.2).
narrative_ontology:measurement(unsc_tr_t30, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 30, 0.22).
narrative_ontology:measurement(unsc_tr_t40, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 40, 0.2).
narrative_ontology:measurement(unsc_tr_t56, unsc_242_withdrawal_clause__partial_withdrawal_reading, theater_ratio, 56, 0.18).

% Extraction over time
narrative_ontology:measurement(unsc_be_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 0, 0.45).
narrative_ontology:measurement(unsc_be_t10, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 10, 0.5).
narrative_ontology:measurement(unsc_be_t20, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 20, 0.55).
narrative_ontology:measurement(unsc_be_t30, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 30, 0.6).
narrative_ontology:measurement(unsc_be_t40, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 40, 0.62).
narrative_ontology:measurement(unsc_be_t56, unsc_242_withdrawal_clause__partial_withdrawal_reading, base_extractiveness, 56, 0.65).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t0, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 0, 0.5).
narrative_ontology:measurement(unsc_su_t10, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 10, 0.55).
narrative_ontology:measurement(unsc_su_t20, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 20, 0.6).
narrative_ontology:measurement(unsc_su_t30, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 30, 0.65).
narrative_ontology:measurement(unsc_su_t40, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 40, 0.68).
narrative_ontology:measurement(unsc_su_t56, unsc_242_withdrawal_clause__partial_withdrawal_reading, suppression_requirement, 56, 0.7).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__partial_withdrawal_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, middle_east_peace_process_framework).
narrative_ontology:affects_constraint(unsc_242_withdrawal_clause__partial_withdrawal_reading, territorial_integrity_principle).

% DUAL FORMULATION NOTE:
% This constraint is one reading of the UNSC Resolution 242 withdrawal clause kernel. Its ε value differs significantly from the 'maximal_withdrawal_reading' due to its interpretation of territorial scope.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
