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
 *   constraint_id: versailles_reparations_clauses__repudiation_reading
 *   human_readable: Versailles Reparations Clauses (Repudiation Reading)
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   This constraint represents the 'repudiation reading' of the Versailles
 *   reparations clauses, asserting that the treaty was illegitimate due to
 *   duress and therefore Germany had no binding obligation to pay reparations
 *   beyond token gestures. This reading was central to German revisionist
 *   policy in the interwar period, leading to a near-total rejection of
 *   payment obligations and maximizing space for German rearmament. The
 *   constraint operates as a snare from the perspective of Allied creditor
 *   states, as it completely suppresses their claims.
 *
 */

/* ==========================================================================
   2. BASE PROPERTIES (DOMAIN PRIORS)
   ========================================================================== */

% --- Numerical metrics ---
domain_priors:base_extractiveness(versailles_reparations_clauses__repudiation_reading, 0.95).
domain_priors:suppression_score(versailles_reparations_clauses__repudiation_reading, 0.9).
domain_priors:theater_ratio(versailles_reparations_clauses__repudiation_reading, 0.1).

% --- Constraint metric facts (engine primary keys, must mirror domain_priors) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, extractiveness, 0.95).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, suppression_requirement, 0.9).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, theater_ratio, 0.1).

% --- NL Profile Metrics (required for mountain constraints) ---
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, accessibility_collapse, 0.9).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, resistance, 0.95).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__repudiation_reading, snare).
narrative_ontology:human_readable(versailles_reparations_clauses__repudiation_reading, "Versailles Reparations Clauses (Repudiation Reading)").
narrative_ontology:topic_domain(versailles_reparations_clauses__repudiation_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__repudiation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__repudiation_reading, 'fa8eda83-59af-40b6-ad82-bdd08f92abb6').
narrative_ontology:cs_kernel_codification('fa8eda83-59af-40b6-ad82-bdd08f92abb6', fixed_text).
narrative_ontology:cs_authority_grounding('fa8eda83-59af-40b6-ad82-bdd08f92abb6', extraction).
narrative_ontology:cs_interpretation_layer_present('fa8eda83-59af-40b6-ad82-bdd08f92abb6').
narrative_ontology:cs_reading_relation('fa8eda83-59af-40b6-ad82-bdd08f92abb6', versailles_reparations_clauses__punitive_liability_reading, forecloses).
narrative_ontology:cs_reading_relation('fa8eda83-59af-40b6-ad82-bdd08f92abb6', versailles_reparations_clauses__limited_responsibility_reading, forecloses).
narrative_ontology:cs_axiom('fa8eda83-59af-40b6-ad82-bdd08f92abb6', foundational, treaty_under_duress_is_void).
narrative_ontology:cs_axiom_status(treaty_under_duress_is_void, holdable).
narrative_ontology:cs_axiom_grounding('fa8eda83-59af-40b6-ad82-bdd08f92abb6', treaty_under_duress_is_void, deontological).
narrative_ontology:cs_axiom('fa8eda83-59af-40b6-ad82-bdd08f92abb6', foundational, national_sovereignty_trumps_imposed_obligations).
narrative_ontology:cs_axiom_status(national_sovereignty_trumps_imposed_obligations, holdable).
narrative_ontology:cs_axiom_grounding('fa8eda83-59af-40b6-ad82-bdd08f92abb6', national_sovereignty_trumps_imposed_obligations, deontological).
narrative_ontology:cs_reference_frame('fa8eda83-59af-40b6-ad82-bdd08f92abb6', unfettered_national_sovereignty).
narrative_ontology:cs_drift_state('fa8eda83-59af-40b6-ad82-bdd08f92abb6', post_versailles_treaty, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('fa8eda83-59af-40b6-ad82-bdd08f92abb6', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_state).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_populace).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, allied_creditor_states).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, war_victims).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The German state, particularly under revisionist governments, actively asserts the illegitimacy of the reparations clauses due to duress. It seeks to minimize or eliminate payments, rearm, and restore full sovereignty, benefiting from the repudiation of financial obligations.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_state, agenda_setter,
    institutional, generational, constrained, national).

% Benefits from the reduction or elimination of reparations payments, which frees up national resources for domestic investment, rearmament, and improved living standards. This reading aligns with nationalistic sentiments and a desire to shed the 'war guilt' clause.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_populace, beneficiary,
    organized, biographical, mobile, national).

% These states (e.g., France, Belgium, UK) are the intended recipients of reparations. Under the repudiation reading, their claims are nullified, and they bear the financial burden of war damages and reconstruction without compensation. Their options are to enforce the treaty by force or concede.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_creditor_states, payer,
    institutional, generational, constrained, global).

% Individuals and communities in Allied nations who suffered direct losses and damages from the war. They are the ultimate beneficiaries of reparations, and their compensation is directly undermined by the repudiation reading, leaving them without recourse.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, war_victims, payer,
    powerless, biographical, trapped, national).

% Analyze the legal validity of treaties signed under duress and the historical context of the Versailles Treaty. Their analysis can either support or challenge the repudiation reading, influencing international opinion and future legal precedents.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, international_legal_scholars, observer,
    analytical, civilizational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: From the perspective of the German state, this reading coordinates national efforts to resist external financial obligations and rebuild national power, aligning domestic political will against foreign demands.
% TRANSFER_FUNCTION: This reading prevents the transfer of wealth from the German state and populace to Allied creditor states and war victims, effectively transferring the burden of war costs back to the victors.
% ABSENT_VOICES: The voices of those who drafted and signed the treaty on behalf of the Allied powers, as well as the direct victims of the war in Allied nations, are absent from this reading's internal logic. They would argue for the treaty's binding nature and Germany's responsibility.
% DISAPPEARANCE_RATIONALE: If the repudiation reading vanished, the German state would be compelled to acknowledge and potentially resume reparations payments, altering its fiscal policy, rearmament plans, and international standing. Allied states would regain a basis for their claims, and the post-WWI international order would be fundamentally re-evaluated.
% FOUNDING_PROBLEM: The German state perceived the Versailles Treaty as an unjust diktat, imposed under threat of continued blockade and invasion, leading to economic hardship and national humiliation.
% FOUNDING_PROBLEM_CORROBORATION: The German state and populace consistently articulated this grievance throughout the interwar period, leading to political instability and the rise of revisionist movements. Historians and political scientists, from outside the benefiting parties, corroborate the widespread perception of duress and injustice within Germany, even if they dispute its legal implications for the treaty's validity.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__repudiation_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__repudiation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__repudiation_reading, '22843cdfd28a814d8f30c35778e75821452545bd',
    '2e9dff2fe8ce0cd758f85569a335a6c41ea42068', '2026-06-13',
    'no_scope_rebuild_gemini', 'agent/example_platform_commission.json',
    'gemini-2.5-flash', 'max_tokens=16384,temperature=0.1,thinking_budget=0').
narrative_ontology:story_seed(versailles_reparations_clauses__repudiation_reading, 'none', 1).

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
 *   The extractiveness is very high (0.95) because this reading aims for near-total non-payment, effectively extracting the full value of the reparations from the creditor states. Suppression is also very high (0.9) as it requires active political and military resistance to Allied enforcement efforts and the suppression of any internal dissent regarding payment. Theater ratio is low (0.1) because the repudiation was a direct and functional policy, not merely performative; any token gestures were minimal and aimed at deflecting immediate pressure rather than fulfilling the spirit of the obligation. Resistance is high (0.95) from the Allied side, as they actively sought to enforce the treaty, but this reading's internal logic is to resist those efforts.
 *
 * PERSPECTIVAL GAP:
 *   From the German perspective, this reading is a necessary act of self-preservation and justice against an illegitimate imposition. From the Allied perspective, it is a breach of international law and a denial of just compensation. The engine's classification will highlight this divergence, showing a snare for the Allied states and a beneficial, if coercive, outcome for Germany.
 *
 * DIRECTIONALITY LOGIC:
 *   The German state and populace are the primary beneficiaries (d near 0.0) as they avoid substantial financial burdens and regain sovereignty. Allied creditor states and war victims are the primary targets (d near 1.0) as their legitimate claims for compensation are nullified. International legal scholars act as observers, analyzing the legal and historical context without direct benefit or cost from the constraint's operation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_duress_legitimacy,
    'To what extent does duress invalidate an international treaty, and was the duress applied to Germany at Versailles sufficient to render the reparations clauses illegitimate under international law?',
    'Analysis of historical precedents and evolving international legal norms regarding treaty validity under coercion, as interpreted by an independent international court.',
    'If duress is deemed sufficient for invalidation, this reading gains legal legitimacy, potentially reclassifying the original reparations as a snare. If not, the repudiation itself becomes a breach of a valid treaty, shifting the burden of illegitimacy.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_duress_legitimacy, conceptual, 'Legal and historical debate over the validity of treaties signed under duress.').

omega_variable(
    reparations_economic_capacity,
    'Was Germany''s economic capacity genuinely insufficient to meet the reparations demands, or was the repudiation primarily a political act to avoid payment?',
    'Independent economic analysis of Germany''s interwar fiscal capacity, industrial output, and capital flight, compared to the actual payment schedules and demands.',
    'If economic capacity was genuinely insufficient, it would lend some empirical support to the ''limited responsibility reading'' and potentially mitigate the ''snare'' aspect of the original demands. If capacity was present, it strengthens the ''repudiation reading'' as a purely political act of extraction avoidance.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reparations_economic_capacity, empirical, 'Empirical question of Germany''s ability to pay reparations versus political will.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__repudiation_reading, 1919, 1939).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1919, 0.1).
narrative_ontology:measurement(vers_tr_t1925, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1925, 0.1).
narrative_ontology:measurement(vers_tr_t1930, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1930, 0.1).
narrative_ontology:measurement(vers_tr_t1935, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1935, 0.1).
narrative_ontology:measurement(vers_tr_t1939, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1939, 0.1).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1919, 0.8).
narrative_ontology:measurement(vers_be_t1925, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1925, 0.85).
narrative_ontology:measurement(vers_be_t1930, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1930, 0.9).
narrative_ontology:measurement(vers_be_t1935, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1935, 0.93).
narrative_ontology:measurement(vers_be_t1939, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1939, 0.95).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1919, 0.7).
narrative_ontology:measurement(vers_su_t1925, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1925, 0.75).
narrative_ontology:measurement(vers_su_t1930, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1930, 0.8).
narrative_ontology:measurement(vers_su_t1935, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1935, 0.85).
narrative_ontology:measurement(vers_su_t1939, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1939, 0.9).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__repudiation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__limited_responsibility_reading).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the Versailles reparations clauses kernel. This 'repudiation reading' asserts the treaty's illegitimacy due to duress, leading to Germany's non-binding obligation for reparations. It directly opposes the 'punitive liability reading' and 'limited responsibility reading' by denying the fundamental obligation.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
