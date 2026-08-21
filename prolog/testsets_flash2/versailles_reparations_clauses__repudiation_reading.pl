% ============================================================================
% CONSTRAINT STORY: versailles_reparations_clauses__repudiation_reading
% ============================================================================
% Version: 1.0 (Deferential Realism Core + Directionality + Boltzmann + Network)
% Logic: 6.0 (Indexed Tuple P,T,E,S + Sigmoid f(d) + Coupling + Purity + Network)
% Generated: 2024-07-30
% Status: [ACTIVE]
% ============================================================================

:- module(constraint_versailles_reparations_clauses__repudiation_reading, []).

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
 *   constraint_id: versailles_reparations_clauses__repudiation_reading
 *   human_readable: Versailles Reparations Clauses (Repudiation Reading)
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'repudiation reading' of the
 *   Versailles Reparations Clauses. This reading asserts that the Treaty of
 *   Versailles was imposed under duress, rendering its reparations clauses
 *   illegitimate and non-binding on Germany beyond token gestures. It
 *   effectively nullifies Allied creditor claims and maximizes German
 *   rearmament space. The high extractiveness (0.95) reflects the complete
 *   suppression of creditor claims from the perspective of the repudiating
 *   party, while high suppression (0.88) indicates the active political and
 *   military efforts required to maintain this stance against international
 *   pressure. The claimed type is 'snare' because the coordination story
 *   (national unity against injustice) is cover for a pure extraction of
 *   resources from Allied nations and the international legal order.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, 0.95).
domain_priors:suppression_score(versailles_reparations_clauses__repudiation_reading, 0.88).
domain_priors:theater_ratio(versailles_reparations_clauses__repudiation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0.88).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, accessibility_collapse, 0.75).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__repudiation_reading, snare).
narrative_ontology:human_readable(versailles_reparations_clauses__repudiation_reading, "Versailles Reparations Clauses (Repudiation Reading)").
narrative_ontology:topic_domain(versailles_reparations_clauses__repudiation_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__repudiation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__repudiation_reading, 'e25c98df-6c07-4980-a5ab-9723d7b18035').
narrative_ontology:cs_kernel_codification('e25c98df-6c07-4980-a5ab-9723d7b18035', fixed_text).
narrative_ontology:cs_authority_grounding('e25c98df-6c07-4980-a5ab-9723d7b18035', extraction).
narrative_ontology:cs_interpretation_layer_present('e25c98df-6c07-4980-a5ab-9723d7b18035').
narrative_ontology:cs_reading_relation('e25c98df-6c07-4980-a5ab-9723d7b18035', versailles_reparations_clauses__punitive_liability_reading, forecloses).
narrative_ontology:cs_reading_relation('e25c98df-6c07-4980-a5ab-9723d7b18035', versailles_reparations_clauses__limited_responsibility_reading, forecloses).
narrative_ontology:cs_axiom('e25c98df-6c07-4980-a5ab-9723d7b18035', foundational, treaty_under_duress_is_void).
narrative_ontology:cs_axiom_status(treaty_under_duress_is_void, holdable).
narrative_ontology:cs_axiom_grounding('e25c98df-6c07-4980-a5ab-9723d7b18035', treaty_under_duress_is_void, deontological).
narrative_ontology:cs_axiom('e25c98df-6c07-4980-a5ab-9723d7b18035', foundational, national_sovereignty_trumps_external_obligations).
narrative_ontology:cs_axiom_status(national_sovereignty_trumps_external_obligations, holdable).
narrative_ontology:cs_axiom_grounding('e25c98df-6c07-4980-a5ab-9723d7b18035', national_sovereignty_trumps_external_obligations, conventional).
narrative_ontology:cs_reference_frame('e25c98df-6c07-4980-a5ab-9723d7b18035', unfettered_national_sovereignty).
narrative_ontology:cs_drift_state('e25c98df-6c07-4980-a5ab-9723d7b18035', post_versailles_signing, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('e25c98df-6c07-4980-a5ab-9723d7b18035', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_nationalists).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_military_industrial_complex).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, allied_creditor_nations).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, international_legal_order).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_citizens_post_ww1).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_citizens_post_ww1).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the narrative of victimhood and injustice, using it to mobilize public support for rearmament and rejection of international obligations. Actively promotes the repudiation reading.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_nationalists, beneficiary,
    organized, generational, mobile, national).

% Benefits from the redirection of funds from reparations to rearmament and industrial expansion. Actively lobbies for policies consistent with the repudiation reading.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_military_industrial_complex, beneficiary,
    powerful, biographical, arbitrage, national).

% Are denied legitimate claims for war damages and security guarantees, bearing the financial and strategic costs of German repudiation. Their options are limited to diplomatic pressure or renewed military action.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_creditor_nations, payer,
    institutional, generational, constrained, global).

% Suffers a blow to its foundational principle of pacta sunt servanda (treaties must be honored), undermining the stability of international agreements and the rule of law. Its 'payment' is the erosion of its own legitimacy.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, international_legal_order, payer,
    analytical, civilizational, identity_locked, universal).

% Are told they are freed from unjust burdens, but also bear the long-term consequences of international isolation and potential conflict arising from repudiation. Their 'benefit' is largely ideological, masking future costs.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_citizens_post_ww1, beneficiary,
    powerless, biographical, trapped, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__repudiation_reading, german_citizens_post_ww1, payer).

% --- OQ-92 receipt surface: who RECEIVES the extraction (capture half).
% 'diffuse' = authored no-capture (piton-side); a seat name = capturer.
% ABSENT field = not authored, fail-closed. Never synthesized. ---
narrative_ontology:stakeholder_gain_flow(versailles_reparations_clauses__repudiation_reading, german_nationalists).
narrative_ontology:fixing_cost_class(versailles_reparations_clauses__repudiation_reading, prohibitive).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates German domestic political will around a narrative of national victimhood and the rejection of external obligations, enabling a unified stance against reparations.
% TRANSFER_FUNCTION: This reading effectively transfers the burden of war reparations from Germany to the Allied creditor nations, while also transferring political capital to nationalist and militarist factions within Germany.
% ABSENT_VOICES: Those within Germany who advocated for compliance with international law or for a more conciliatory approach to international relations were suppressed or marginalized by the dominant nationalist narrative. Their voices would have argued for the long-term benefits of international cooperation over short-term repudiation.
% DISAPPEARANCE_RATIONALE: If this repudiation reading vanished, Germany's domestic political landscape would be fundamentally altered, requiring a re-evaluation of its international obligations and potentially leading to a more cooperative, less militaristic foreign policy. Allied nations would reassert their claims, and the international legal order would regain some stability.
% FOUNDING_PROBLEM: The problem this reading was 'built to solve' was the perceived injustice and economic burden of the Treaty of Versailles on Germany, particularly the reparations clauses and the 'war guilt' clause (Article 231).
% FOUNDING_PROBLEM_CORROBORATION: German nationalist historians and political figures attest that the problem of an unjust treaty remains live. Allied historians and international legal scholars, from outside the benefiting parties, largely corroborate the historical burden but dispute the legitimacy of unilateral repudiation as a solution, arguing it created new problems.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__repudiation_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__repudiation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__repudiation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini2', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(versailles_reparations_clauses__repudiation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__repudiation_reading, 0.95, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__repudiation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

:- end_tests(versailles_reparations_clauses__repudiation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high because this reading aims for a near-total transfer of the reparations burden away from Germany. Suppression is also very high, as this reading requires active political propaganda, diplomatic maneuvering, and eventually military rearmament to resist and ultimately overturn the treaty obligations. The theater ratio is low because the repudiation is a direct, active policy, not a performative maintenance of an atrophied function. Resistance is high because this reading directly challenges the established international order and the interests of powerful states.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of German nationalists, this reading is a just and necessary act of national liberation. From the perspective of Allied creditor nations, it is a blatant violation of international law and a dangerous act of aggression. The engine's classification as a snare reflects the structural reality of extraction and suppression, regardless of the self-justifying narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   German nationalists and the military-industrial complex are clear beneficiaries, as they gain political power and resources by rejecting reparations. Allied creditor nations and the international legal order are the primary victims, as their claims are nullified and their foundational principles undermined. German citizens are complex: ideologically 'benefiting' from perceived liberation, but ultimately paying the price of international instability. The repudiation reading is a structural mechanism for Germany to extract itself from obligations at the expense of others.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    duress_legitimacy_threshold,
    'At what point does ''duress'' in treaty negotiation render a treaty illegitimate under international law, and was the Treaty of Versailles beyond that threshold?',
    'Analysis of historical diplomatic records and legal precedents regarding treaty formation under post-conflict conditions, focusing on the definition of ''duress'' in early 20th-century international law.',
    'If the duress claim is legally validated, it would lend some legitimacy to the repudiation reading, potentially reclassifying parts of the constraint as a contested ''tangled_rope'' rather than a pure ''snare''. If not, the repudiation remains a unilateral act of extraction.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(duress_legitimacy_threshold, conceptual, 'Legal interpretation of duress in treaty law and its application to Versailles.').

omega_variable(
    economic_capacity_vs_political_will,
    'To what extent was Germany''s inability to pay reparations a genuine economic constraint versus a political choice driven by the repudiation narrative?',
    'Counterfactual economic modeling of German capacity under different political regimes, and analysis of historical budget allocations (e.g., military spending vs. reparations payments).',
    'If economic capacity was genuinely prohibitive, it would shift some of the ''extraction'' burden from the repudiation reading to a ''mountain'' of economic reality, making the constraint less of a snare. If political will was the primary driver, the snare classification is reinforced.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(economic_capacity_vs_political_will, empirical, 'Distinguishing economic limits from political choices in reparations payments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__repudiation_reading, 1919, 1939).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1919, 0.05).
narrative_ontology:measurement(vers_tr_t1925, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1925, 0.08).
narrative_ontology:measurement(vers_tr_t1930, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1930, 0.1).
narrative_ontology:measurement(vers_tr_t1935, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1935, 0.1).
narrative_ontology:measurement(vers_tr_t1939, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1939, 0.1).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1919, 0.7).
narrative_ontology:measurement(vers_be_t1925, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1925, 0.8).
narrative_ontology:measurement(vers_be_t1930, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1930, 0.88).
narrative_ontology:measurement(vers_be_t1935, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1935, 0.92).
narrative_ontology:measurement(vers_be_t1939, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1939, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1919, 0.6).
narrative_ontology:measurement(vers_su_t1925, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1925, 0.7).
narrative_ontology:measurement(vers_su_t1930, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1930, 0.8).
narrative_ontology:measurement(vers_su_t1935, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1935, 0.85).
narrative_ontology:measurement(vers_su_t1939, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1939, 0.88).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__repudiation_reading, identity_coordination).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, locarno_treaties).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, dawes_plan).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, young_plan).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, german_rearmament_program).

% DUAL FORMULATION NOTE:
% This is one of three readings of the 'versailles_reparations_clauses' kernel. This 'repudiation_reading' directly challenges the legitimacy of the entire reparations framework, contrasting with the 'punitive_liability_reading' (which emphasizes Germany's moral and financial responsibility) and the 'limited_responsibility_reading' (which focuses on German economic capacity).

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
