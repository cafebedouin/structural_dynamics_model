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
    narrative_ontology:constraint_vindicates/2,
    narrative_ontology:affects_constraint/2,
    narrative_ontology:coordination_type/2,
    constraint_indexing:constraint_classification/3,
    narrative_ontology:constraint_stakeholder/7,
    narrative_ontology:disappearance_verdict/2,
    narrative_ontology:founding_problem_status/2,
    domain_priors:emerges_naturally/1,
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
 *   human_readable: Repudiation of Versailles Reparations (German Reading)
 *   domain: International Relations / Legal History / Political Economy
 *
 * SUMMARY:
 *   This constraint story models the 'repudiation reading' of the Treaty of
 *   Versailles' reparations clauses, prevalent in Germany during the interwar
 *   period. This reading asserts that the treaty, imposed under duress, was
 *   fundamentally illegitimate, and therefore Germany had no binding
 *   obligation to pay reparations beyond token gestures. The claim is framed
 *   as a 'mountain' (inherent illegitimacy) but its operation involves active
 *   enforcement (resistance to payment) and clear beneficiaries (German state
 *   and populace) and victims (Allied creditors). The divergence between the
 *   claimed type and the operational metrics is central to its analysis.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, 0.9).
domain_priors:suppression_score(versailles_reparations_clauses__repudiation_reading, 0.85).
domain_priors:theater_ratio(versailles_reparations_clauses__repudiation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, extractiveness, 0.9).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0.85).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, accessibility_collapse, 0.8).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__repudiation_reading, mountain).
narrative_ontology:human_readable(versailles_reparations_clauses__repudiation_reading, "Repudiation of Versailles Reparations (German Reading)").
narrative_ontology:topic_domain(versailles_reparations_clauses__repudiation_reading, "International Relations / Legal History / Political Economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__repudiation_reading).
domain_priors:emerges_naturally(versailles_reparations_clauses__repudiation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__repudiation_reading, '92a086b1-9e2e-4ad8-92a0-fb0af32bb71f').
narrative_ontology:cs_kernel_codification('92a086b1-9e2e-4ad8-92a0-fb0af32bb71f', fixed_text).
narrative_ontology:cs_authority_grounding('92a086b1-9e2e-4ad8-92a0-fb0af32bb71f', extraction).
narrative_ontology:cs_interpretation_layer_present('92a086b1-9e2e-4ad8-92a0-fb0af32bb71f').
narrative_ontology:cs_reading_relation('92a086b1-9e2e-4ad8-92a0-fb0af32bb71f', versailles_reparations_clauses__punitive_liability_reading, forecloses).
narrative_ontology:cs_reading_relation('92a086b1-9e2e-4ad8-92a0-fb0af32bb71f', versailles_reparations_clauses__limited_responsibility_reading, forecloses).
narrative_ontology:cs_axiom('92a086b1-9e2e-4ad8-92a0-fb0af32bb71f', foundational, treaty_under_duress_is_void).
narrative_ontology:cs_axiom_status(treaty_under_duress_is_void, holdable).
narrative_ontology:cs_axiom_grounding('92a086b1-9e2e-4ad8-92a0-fb0af32bb71f', treaty_under_duress_is_void, deontological).
narrative_ontology:cs_axiom('92a086b1-9e2e-4ad8-92a0-fb0af32bb71f', foundational, national_sovereignty_overrides_imposed_obligations).
narrative_ontology:cs_axiom_status(national_sovereignty_overrides_imposed_obligations, holdable).
narrative_ontology:cs_axiom_grounding('92a086b1-9e2e-4ad8-92a0-fb0af32bb71f', national_sovereignty_overrides_imposed_obligations, conventional).
narrative_ontology:cs_reference_frame('92a086b1-9e2e-4ad8-92a0-fb0af32bb71f', sovereign_equality_and_free_consent).
narrative_ontology:cs_drift_state('92a086b1-9e2e-4ad8-92a0-fb0af32bb71f', post_versailles_era, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('92a086b1-9e2e-4ad8-92a0-fb0af32bb71f', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_state).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_populace).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, allied_creditor_states).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, french_populace).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, belgian_populace).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, duress_invalidates_treaties_doctrine).
narrative_ontology:constraint_vindicates(versailles_reparations_clauses__repudiation_reading, national_self_determination_principle).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The political entity asserting the illegitimacy of the treaty and actively resisting payment, seeking to maximize its rearmament space and economic recovery by rejecting external obligations. Benefits from non-payment.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_state, agenda_setter,
    institutional, generational, constrained, national).

% The citizens of Germany, who largely perceive the treaty as an unjust 'Diktat' and benefit from the state's repudiation of reparations, experiencing reduced economic burden and a sense of restored national dignity. Their identity is tied to this narrative of injustice.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_populace, beneficiary,
    organized, biographical, identity_locked, national).

% The nations (e.g., France, Britain, Belgium) that were owed reparations under the Treaty of Versailles. They bear the cost of non-payment and face the challenge of enforcing a treaty obligation against a sovereign state's determined resistance. Their options are diplomatic pressure, sanctions, or military action, all with high costs.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_creditor_states, payer,
    institutional, generational, constrained, global).

% Citizens of France, who suffered immense damage during WWI and expected reparations to fund reconstruction and compensate for losses. They bear the indirect cost of German non-payment through higher taxes or slower recovery, and are constrained by their government's ability to enforce the treaty.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, french_populace, payer,
    organized, biographical, constrained, national).

% Citizens of Belgium, whose country was heavily occupied and devastated during WWI. They, like the French, expected reparations and bear the cost of non-payment, constrained by the geopolitical realities of the interwar period.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, belgian_populace, payer,
    organized, biographical, constrained, national).

% Academics and legal experts who analyze the legality of the Treaty of Versailles, the concept of duress in international law, and the implications of repudiation for the international legal order. They observe the structural dynamics without direct participation.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: To coordinate the German state and populace in a unified rejection of the Treaty of Versailles' reparations clauses, asserting the treaty's illegitimacy due to duress and thereby nullifying perceived binding obligations.
% TRANSFER_FUNCTION: Prevents the transfer of significant economic resources (reparations payments) from Germany to the Allied creditor states, effectively transferring the burden of war reconstruction back to the Allied nations.
% ABSENT_VOICES: Allied populations demanding compensation for war damages, international legal bodies upholding the sanctity of treaties (pacta sunt servanda), and those advocating for a stable, rules-based international order. These voices are actively resisted or dismissed by the repudiation narrative.
% DISAPPEARANCE_RATIONALE: If this reading (the belief in the treaty's illegitimacy and the right to repudiate) vanished, Germany would likely face renewed pressure to pay reparations, potentially altering its economic recovery, rearmament trajectory, and relationship with Allied powers, fundamentally reshaping interwar European politics.
% FOUNDING_PROBLEM: The perceived unjust and punitive nature of the Treaty of Versailles, particularly Article 231 (the 'war guilt clause') and the resulting reparations burden, which was seen by many Germans as economically crippling, morally humiliating, and imposed under duress.
% FOUNDING_PROBLEM_CORROBORATION: German historians, politicians, and public opinion during the interwar period widely attested to the live nature of this problem. Some contemporary revisionist historians and legal scholars also support the view that the treaty's terms were excessively harsh and its imposition problematic. Allied historians and legal scholars would largely contest this framing, asserting Germany's responsibility and the treaty's legality.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__repudiation_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__repudiation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__repudiation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=8192').
narrative_ontology:story_seed(versailles_reparations_clauses__repudiation_reading, 'none', 1).
narrative_ontology:epsilon_provenance(versailles_reparations_clauses__repudiation_reading, 0.9, 'gemini-2.5-flash', 'none', direct).

/* ==========================================================================
   4. VALIDATION TESTS
   ========================================================================== */

:- begin_tests(versailles_reparations_clauses__repudiation_reading_tests).

test(extraction_signature) :-
    domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, E),
    E >= 0.46. % Ensures high-extraction Snare/Tangled territory.

% OQ-194: diagnostic probe, NOT a gate. Failure here means the authored
% mountain claim diverges from the story's computed metrics (claim != actual
% is the DR core) -- contested/extractive territory, not a regression. Bars
% (E=<0.25, S=<0.05, AC>=0.85, R=<0.15) are hardcoded; recalibration -> OQ-48.
test(mountain_threshold_validation) :-
    config:param(extractiveness_metric_name, ExtMetricName),
    narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, ExtMetricName, E),
    domain_priors:suppression_score(versailles_reparations_clauses__repudiation_reading, S),
    E =< 0.25,
    S =< 0.05.

test(nl_profile_validation) :-
    domain_priors:emerges_naturally(versailles_reparations_clauses__repudiation_reading),
    narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, accessibility_collapse, AC),
    narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, resistance, R),
    AC >= 0.85,
    R =< 0.15.

:- end_tests(versailles_reparations_clauses__repudiation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The high extractiveness (0.9) reflects the complete suppression of Allied creditor claims from the German perspective, effectively extracting wealth from the Allied nations by preventing its transfer. Suppression (0.85) is high because this reading requires active political and diplomatic resistance to Allied demands and internal coordination to maintain non-payment. Theater ratio is low (0.1) because the repudiation is a direct, unambiguous political stance, not a performance masking a different function. Accessibility collapse (0.8) is high because this reading aims to completely collapse the legitimacy of Allied claims. Resistance (0.9) is high because this reading itself is a form of resistance against the treaty's imposition. The measurement series show a hardening of this position over time, reflecting the increasing political will to reject the treaty.
 *
 * PERSPECTIVAL GAP:
 *   From the German perspective, the constraint is a legitimate assertion of national sovereignty and a correction of an unjust imposition. From the Allied perspective, it is a breach of international law and a unilateral act of extraction. The engine's classification will highlight this divergence by evaluating the 'mountain' claim against the high extractiveness and suppression metrics.
 *
 * DIRECTIONALITY LOGIC:
 *   The German state and populace are the primary beneficiaries (d near 0.0), as they avoid substantial financial burdens and gain political leverage. The Allied creditor states and their populations are the primary targets/victims (d near 1.0), as they are denied expected compensation. International legal scholars act as analytical observers. The 'emerges_naturally: true' flag, combined with declared beneficiaries, triggers the False Summit Mountain detection, indicating a claim of natural law that benefits specific parties.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    duress_legitimacy_ambiguity,
    'Is the claim of ''duress'' a universally recognized legal principle for invalidating treaties, or a political justification for repudiation?',
    'Analysis of international legal precedents and scholarly consensus on treaty law (Vienna Convention on the Law of Treaties, though later, reflects evolving principles).',
    'If duress is a universally valid ground, the repudiation reading gains legal force, supporting its ''mountain'' claim. If it''s primarily a political justification, the constraint is more clearly a ''snare'' for creditors, sustained by German political will rather than inherent illegitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(duress_legitimacy_ambiguity, conceptual, 'Ambiguity between legal principle and political justification for treaty invalidation.').

omega_variable(
    repudiation_as_precursor_to_aggression,
    'Was the repudiation of reparations primarily an act of national self-determination and economic recovery, or a strategic precursor to rearmament and future aggression?',
    'Historical analysis of German foreign policy, military spending, and diplomatic communications from 1919-1939, including internal government documents.',
    'If primarily self-determination, the repudiation is a ''rope'' for German internal coordination. If a precursor to aggression, it functions as a ''snare'' for the international system, extracting security and stability from Allied powers under false pretenses.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(repudiation_as_precursor_to_aggression, empirical, 'Whether repudiation was defensive or a prelude to offensive action.').

omega_variable(
    false_summit_mountain_ambiguity,
    'Is the illegitimacy of the Versailles Treaty a genuine natural law (a ''mountain''), or a constructed claim that benefits identifiable agents (a ''tangled_rope'' or ''snare'')?',
    'Analysis of the historical context, the legal arguments for and against duress, and the direct beneficiaries of the repudiation. If the ''naturalness'' of illegitimacy is primarily asserted by those who benefit from it, it suggests a constructed constraint.',
    'If a genuine natural law, the classification as ''mountain'' holds. If constructed, the constraint would reclassify to a ''tangled_rope'' or ''snare'' from the perspective of the international system, highlighting the extractive nature of the claim.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(false_summit_mountain_ambiguity, conceptual, 'Ambiguity between genuine natural law and constructed claim for the treaty''s illegitimacy.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__repudiation_reading, 1919, 1933).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1919, 0.1).
narrative_ontology:measurement(vers_tr_t1923, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1923, 0.1).
narrative_ontology:measurement(vers_tr_t1926, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1926, 0.1).
narrative_ontology:measurement(vers_tr_t1929, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1929, 0.1).
narrative_ontology:measurement(vers_tr_t1933, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1933, 0.1).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1919, 0.7).
narrative_ontology:measurement(vers_be_t1923, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1923, 0.78).
narrative_ontology:measurement(vers_be_t1926, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1926, 0.83).
narrative_ontology:measurement(vers_be_t1929, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1929, 0.87).
narrative_ontology:measurement(vers_be_t1933, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1933, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1919, 0.65).
narrative_ontology:measurement(vers_su_t1923, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1923, 0.72).
narrative_ontology:measurement(vers_su_t1926, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1926, 0.78).
narrative_ontology:measurement(vers_su_t1929, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1929, 0.82).
narrative_ontology:measurement(vers_su_t1933, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1933, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__repudiation_reading, identity_coordination).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__limited_responsibility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'versailles_reparations_clauses' kernel, each representing a distinct structural claim regarding the legitimacy and enforceability of reparations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
