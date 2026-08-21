% ============================================================================
% CONSTRAINT STORY: unsc_242_withdrawal_clause__maximal_withdrawal_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_unsc_242_withdrawal_clause__maximal_withdrawal_reading, []).

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
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:stakeholder_non_agent/2,
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
 *   constraint_id: unsc_242_withdrawal_clause__maximal_withdrawal_reading
 *   human_readable: UNSC Resolution 242 Withdrawal Clause: Maximal Interpretation
 *   domain: international_law/diplomacy
 *
 * SUMMARY:
 *   This constraint represents the maximal interpretation of UN Security
 *   Council Resolution 242's withdrawal clause, asserting that withdrawal is
 *   mandatory from all occupied territories, as implied by the French
 *   definite article 'les territoires' and the UN Charter's Article 2(4)
 *   principle of territorial integrity. This reading binds the occupying
 *   state to full retrocession, establishing an enforceable legal position
 *   for dispossessed claimants. The constraint is claimed as a Tangled Rope
 *   because it serves a genuine coordination function for the international
 *   legal order and dispossessed claimants (upholding territorial integrity)
 *   while simultaneously extracting territory and sovereignty from the
 *   occupying state, requiring active enforcement to persist.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.85).
domain_priors:suppression_score(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.75).
domain_priors:theater_ratio(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.15).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, extractiveness, 0.85).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 0.75).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 0.15).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(unsc_242_withdrawal_clause__maximal_withdrawal_reading, resistance, 0.7).

% --- Constraint claim ---
narrative_ontology:constraint_claim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, tangled_rope).
narrative_ontology:human_readable(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "UNSC Resolution 242 Withdrawal Clause: Maximal Interpretation").
narrative_ontology:topic_domain(unsc_242_withdrawal_clause__maximal_withdrawal_reading, "international_law/diplomacy").

domain_priors:requires_active_enforcement(unsc_242_withdrawal_clause__maximal_withdrawal_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'c467ca46-60c0-42a9-b89f-e6e8d482ca35').
narrative_ontology:cs_kernel_codification('c467ca46-60c0-42a9-b89f-e6e8d482ca35', fixed_text).
narrative_ontology:cs_authority_grounding('c467ca46-60c0-42a9-b89f-e6e8d482ca35', lineage).
narrative_ontology:cs_interpretation_layer_present('c467ca46-60c0-42a9-b89f-e6e8d482ca35').
narrative_ontology:cs_reading_relation('c467ca46-60c0-42a9-b89f-e6e8d482ca35', unsc_242_withdrawal_clause__partial_withdrawal_reading, forecloses).
narrative_ontology:cs_reading_relation('c467ca46-60c0-42a9-b89f-e6e8d482ca35', unsc_242_withdrawal_clause__interpretive_authority_structure, coexists_with).
narrative_ontology:cs_axiom('c467ca46-60c0-42a9-b89f-e6e8d482ca35', foundational, territorial_integrity_is_absolute).
narrative_ontology:cs_axiom_status(territorial_integrity_is_absolute, holdable).
narrative_ontology:cs_axiom_grounding('c467ca46-60c0-42a9-b89f-e6e8d482ca35', territorial_integrity_is_absolute, deontological).
narrative_ontology:cs_axiom('c467ca46-60c0-42a9-b89f-e6e8d482ca35', secondary, definite_article_controls_scope).
narrative_ontology:cs_axiom_status(definite_article_controls_scope, holdable).
narrative_ontology:cs_axiom_grounding('c467ca46-60c0-42a9-b89f-e6e8d482ca35', definite_article_controls_scope, conventional).
narrative_ontology:cs_reference_frame('c467ca46-60c0-42a9-b89f-e6e8d482ca35', un_charter_territorial_integrity).
narrative_ontology:cs_drift_state('c467ca46-60c0-42a9-b89f-e6e8d482ca35', contemporary_geopolitical_context, gap(practice_drift, substantial, true)).
narrative_ontology:cs_created_at('c467ca46-60c0-42a9-b89f-e6e8d482ca35', '').
narrative_ontology:cs_kernel_id(unsc_242_withdrawal_clause__maximal_withdrawal_reading, unsc_242_withdrawal_clause).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_claimants).
narrative_ontology:constraint_beneficiary(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_legal_order).
narrative_ontology:constraint_victim(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% As the state occupying territories since 1967, it is mandated by this reading to withdraw from all such territories. This entails significant political, economic, and security costs, and it actively resists full implementation, citing security concerns and alternative interpretations.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, occupying_state, payer,
    institutional, generational, constrained, regional).

% Representing the populations and states whose territories are occupied, they are the primary beneficiaries of the maximal withdrawal mandate, gaining a clear legal basis for retrocession and restoration of territorial integrity. Their ability to enforce this claim is constrained by geopolitical realities.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_claimants, beneficiary,
    organized, generational, constrained, regional).

% The body that adopted Resolution 242, it holds the ultimate responsibility for its implementation and enforcement. Its authority is foundational to the constraint, though its capacity for enforcement is subject to the political will of its permanent members.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, un_security_council, agenda_setter,
    institutional, civilizational, mobile, global).

% Academics and legal experts who analyze the resolution's text, drafting history, and subsequent state practice. They provide critical interpretations, including the maximal withdrawal reading, which informs international discourse and judicial opinions.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% Judges of the International Court of Justice, who claim the authority to provide definitive legal interpretations of UN resolutions and international law. Their rulings, if sought and accepted, would authoritatively clarify the scope of withdrawal.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, icj_judges, agenda_setter,
    institutional, civilizational, analytical, global).

% The overarching system of international law and norms benefits from the maximal reading, as it reinforces the fundamental principle of territorial integrity and the inadmissibility of acquiring territory by force, thereby enhancing the stability and legitimacy of the global legal framework.
narrative_ontology:constraint_stakeholder(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_legal_order, beneficiary,
    institutional, civilizational, analytical, global).
narrative_ontology:stakeholder_non_agent(unsc_242_withdrawal_clause__maximal_withdrawal_reading, international_legal_order).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(unsc_242_withdrawal_clause__maximal_withdrawal_reading, dispossessed_claimants).
narrative_ontology:fixing_cost_class(unsc_242_withdrawal_clause__maximal_withdrawal_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Establishes a clear, universally applicable legal standard for the non-acquisition of territory by force, coordinating international state behavior around territorial integrity and the peaceful resolution of disputes.
% TRANSFER_FUNCTION: Mandates the transfer of sovereignty and control over occupied territories from the occupying state to the dispossessed claimants, upholding the principle of territorial integrity as enshrined in the UN Charter.
% ABSENT_VOICES: Populations under occupation, particularly those whose self-determination claims might extend beyond the restoration of pre-1967 borders, often lack direct representation in the high-level diplomatic and legal forums where Resolution 242 is debated and interpreted.
% DISAPPEARANCE_RATIONALE: If the maximal withdrawal mandate vanished, the international legal order's foundational principle of territorial integrity would be severely undermined, potentially legitimizing acquisition by force and leading to widespread territorial disputes, instability, and a breakdown of trust in international institutions.
% FOUNDING_PROBLEM: The acquisition of territory by force by Israel in the 1967 Arab-Israeli War, which challenged the post-WWII international legal order's prohibition on such actions and created a protracted conflict over occupied lands.
% FOUNDING_PROBLEM_CORROBORATION: The international community, including the UN General Assembly, the International Court of Justice, and numerous states, consistently reaffirms the principle of inadmissibility of acquisition of territory by force and the applicability of the Fourth Geneva Convention to occupied territories, corroborating the ongoing relevance of the founding problem. The continued occupation itself serves as corroboration.
narrative_ontology:disappearance_verdict(unsc_242_withdrawal_clause__maximal_withdrawal_reading, world_rearranges).
narrative_ontology:founding_problem_status(unsc_242_withdrawal_clause__maximal_withdrawal_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 'none', 1).
narrative_ontology:epsilon_provenance(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 0.85, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(unsc_242_withdrawal_clause__maximal_withdrawal_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(unsc_242_withdrawal_clause__maximal_withdrawal_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.85) reflects the comprehensive nature of the withdrawal demand, imposing significant costs on the occupying state. Suppression (0.75) is substantial due to the weight of international law and the UN Security Council's authority, which actively seeks to enforce the resolution, though often constrained by political factors. The low theater ratio (0.15) indicates that, from this maximal reading's perspective, the obligation is clear and not merely performative, even if implementation is resisted. The increasing extractiveness and suppression over time reflect the hardening of international legal consensus and ongoing pressure for full implementation, despite continued occupation.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the dispossessed claimants and the international legal order, this constraint functions as a legitimate and necessary mechanism for upholding fundamental principles. However, from the occupying state's perspective, it is a highly extractive and coercive demand that disregards its security concerns and alternative interpretations of the resolution. The engine's per-seat classification will capture this divergence.
 *
 * DIRECTIONALITY LOGIC:
 *   The dispossessed claimants and the international legal order are clear beneficiaries (d near 0.0) as the constraint directly supports their claims and principles. The occupying state is the primary target (d near 1.0) as it bears the full cost of withdrawal. The UN Security Council acts as the agenda-setter, responsible for the constraint's creation and enforcement, balancing coordination and extraction.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    textual_ambiguity_resolution,
    'Does the French definite article ''les territoires'' definitively mandate withdrawal from all occupied territories, or does the English indefinite article ''territories'' allow for partial withdrawal based on security considerations?',
    'A definitive ruling by the International Court of Justice on the authoritative text and its interpretation, or a new UN Security Council resolution clarifying the scope.',
    'If the English text is deemed authoritative or allows for discretion, the constraint''s extractiveness would decrease, potentially reclassifying it towards a Rope or even a Piton if the mandate becomes purely performative. If the French text''s maximal interpretation is universally accepted, the constraint''s Tangled Rope nature would be reinforced.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(textual_ambiguity_resolution, conceptual, 'Ambiguity in the English vs. French texts of Resolution 242 regarding the scope of withdrawal.').

omega_variable(
    enforcement_political_will,
    'To what extent is the maximal withdrawal mandate''s persistence and effectiveness dependent on the political will of powerful states within the UN Security Council, rather than its inherent legal force?',
    'Analysis of historical instances where Security Council resolutions were either fully enforced or ignored, correlating enforcement outcomes with the geopolitical interests of permanent members.',
    'If enforcement is primarily a function of political will, the constraint''s effective suppression and extractiveness are highly variable and contingent, potentially shifting its classification towards a Piton (if ignored) or a Snare (if selectively enforced for political gain). If its legal force consistently compels action, its Tangled Rope classification is more robust.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(enforcement_political_will, empirical, 'The role of political will in the enforcement of international legal mandates.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(unsc_242_withdrawal_clause__maximal_withdrawal_reading, 1967, 2027).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(unsc_tr_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1967, 0.1).
narrative_ontology:measurement(unsc_tr_t1977, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1977, 0.12).
narrative_ontology:measurement(unsc_tr_t1987, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1987, 0.13).
narrative_ontology:measurement(unsc_tr_t1997, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 1997, 0.14).
narrative_ontology:measurement(unsc_tr_t2007, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2007, 0.14).
narrative_ontology:measurement(unsc_tr_t2017, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2017, 0.15).
narrative_ontology:measurement(unsc_tr_t2027, unsc_242_withdrawal_clause__maximal_withdrawal_reading, theater_ratio, 2027, 0.15).

% Extraction over time
narrative_ontology:measurement(unsc_be_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1967, 0.7).
narrative_ontology:measurement(unsc_be_t1977, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1977, 0.75).
narrative_ontology:measurement(unsc_be_t1987, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1987, 0.8).
narrative_ontology:measurement(unsc_be_t1997, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 1997, 0.82).
narrative_ontology:measurement(unsc_be_t2007, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2007, 0.83).
narrative_ontology:measurement(unsc_be_t2017, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2017, 0.84).
narrative_ontology:measurement(unsc_be_t2027, unsc_242_withdrawal_clause__maximal_withdrawal_reading, base_extractiveness, 2027, 0.85).

% Suppression requirement over time
narrative_ontology:measurement(unsc_su_t1967, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1967, 0.65).
narrative_ontology:measurement(unsc_su_t1977, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1977, 0.68).
narrative_ontology:measurement(unsc_su_t1987, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1987, 0.7).
narrative_ontology:measurement(unsc_su_t1997, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 1997, 0.72).
narrative_ontology:measurement(unsc_su_t2007, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2007, 0.73).
narrative_ontology:measurement(unsc_su_t2017, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2017, 0.74).
narrative_ontology:measurement(unsc_su_t2027, unsc_242_withdrawal_clause__maximal_withdrawal_reading, suppression_requirement, 2027, 0.75).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(unsc_242_withdrawal_clause__maximal_withdrawal_reading, enforcement_mechanism).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
