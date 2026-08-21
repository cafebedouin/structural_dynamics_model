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
    narrative_ontology:omega_variable/3,
    narrative_ontology:cs_story_uid/2,
    narrative_ontology:cs_kernel_codification/2,
    narrative_ontology:cs_authority_grounding/2,
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
 *   This constraint represents the 'repudiation reading' of the Versailles
 *   Treaty's reparations clauses, prevalent in interwar Germany. It asserts
 *   that the treaty, having been imposed under duress, lacked legitimacy and
 *   therefore Germany had no binding obligation to pay reparations beyond
 *   token gestures. This reading served to justify unilateral non-compliance
 *   and rearmament, leading to high extraction from Allied creditors and
 *   significant resistance from the international legal order. The claimed
 *   type is 'snare' because the coordination story (national unity against
 *   injustice) is cover for pure extraction from the Allied nations,
 *   maintained by active suppression of their claims.
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
narrative_ontology:cs_story_uid(versailles_reparations_clauses__repudiation_reading, 'c216cff4-8b99-4ef6-8031-3f1f26b3c029').
narrative_ontology:cs_kernel_codification('c216cff4-8b99-4ef6-8031-3f1f26b3c029', fixed_text).
narrative_ontology:cs_authority_grounding('c216cff4-8b99-4ef6-8031-3f1f26b3c029', extraction).
narrative_ontology:cs_reading_relation('c216cff4-8b99-4ef6-8031-3f1f26b3c029', versailles_reparations_clauses__punitive_liability_reading, forecloses).
narrative_ontology:cs_reading_relation('c216cff4-8b99-4ef6-8031-3f1f26b3c029', versailles_reparations_clauses__limited_responsibility_reading, forecloses).
narrative_ontology:cs_axiom('c216cff4-8b99-4ef6-8031-3f1f26b3c029', foundational, treaty_under_duress_is_void).
narrative_ontology:cs_axiom_status(treaty_under_duress_is_void, holdable).
narrative_ontology:cs_axiom_grounding('c216cff4-8b99-4ef6-8031-3f1f26b3c029', treaty_under_duress_is_void, deontological).
narrative_ontology:cs_axiom('c216cff4-8b99-4ef6-8031-3f1f26b3c029', foundational, national_sovereignty_trumps_imposed_obligations).
narrative_ontology:cs_axiom_status(national_sovereignty_trumps_imposed_obligations, holdable).
narrative_ontology:cs_axiom_grounding('c216cff4-8b99-4ef6-8031-3f1f26b3c029', national_sovereignty_trumps_imposed_obligations, deontological).
narrative_ontology:cs_reference_frame('c216cff4-8b99-4ef6-8031-3f1f26b3c029', national_sovereignty_unconstrained).
narrative_ontology:cs_drift_state('c216cff4-8b99-4ef6-8031-3f1f26b3c029', post_versailles_treaty, gap(repudiation_pressure, severe, true)).
narrative_ontology:cs_created_at('c216cff4-8b99-4ef6-8031-3f1f26b3c029', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_nationalists).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_military_industrial_complex).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, allied_creditor_nations).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, international_legal_order).
% Derived from stakeholders[] roles (beneficiary->beneficiary, payer->victim;
% agent-gated; excluded derives nothing; deduped against the authored arrays).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_populace).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, german_populace).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% Benefits from the narrative of victimhood and injustice, using it to mobilize public support for rearmament and rejection of international obligations. This reading provides ideological cover for their agenda.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_nationalists, beneficiary,
    organized, generational, mobile, national).

% Directly benefits from the repudiation of reparations, as funds are freed for rearmament and industrial expansion. This reading removes financial constraints on their growth.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_military_industrial_complex, beneficiary,
    powerful, biographical, arbitrage, national).

% Are denied the reparations they believe are legally and morally owed, leading to economic instability and political resentment. Their options are to enforce the treaty by force (risky) or accept repudiation (costly).
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_creditor_nations, payer,
    institutional, generational, constrained, continental).

% Suffers a severe blow to its legitimacy and enforceability when a major treaty is unilaterally repudiated. This undermines the principle of pacta sunt servanda and encourages future breaches.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, international_legal_order, payer,
    institutional, civilizational, trapped, global).

% Benefits from the psychological relief of rejecting perceived unjust burdens and from resources diverted from reparations to domestic spending. However, they also bear the long-term costs of international isolation and potential conflict.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_populace, beneficiary,
    moderate, biographical, identity_locked, national).
narrative_ontology:stakeholder_secondary_role(versailles_reparations_clauses__repudiation_reading, german_populace, payer).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: This reading coordinates German national sentiment around a shared grievance, enabling collective action to reject external obligations and pursue national interests unconstrained by the treaty.
% TRANSFER_FUNCTION: This reading prevents the transfer of wealth from Germany to Allied creditor nations, effectively transferring the burden of war costs back to the creditors and freeing German resources for domestic use and rearmament.
% ABSENT_VOICES: The voices of international legal scholars who uphold the validity of treaties, and the populations of Allied nations who suffered immense losses and expected compensation, are dismissed as biased or irrelevant by this reading.
% DISAPPEARANCE_RATIONALE: If this repudiation reading vanished, Germany would likely face renewed pressure to honor its obligations, potentially leading to economic strain but also greater international integration. The geopolitical landscape of interwar Europe would have been fundamentally different, with less impetus for aggressive rearmament.
% FOUNDING_PROBLEM: The problem this reading 'solves' is the perceived injustice and economic burden of the Versailles Treaty's reparations clauses on Germany, which were seen as an illegitimate imposition under duress.
% FOUNDING_PROBLEM_CORROBORATION: German nationalists and revisionist historians attest the problem is live. Allied nations and mainstream international legal scholars dispute this, arguing the treaty was a legitimate outcome of war and the problem is manufactured to justify non-compliance.
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__repudiation_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__repudiation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__repudiation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini3', 'agent/example_platform_commission.json',
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
 *   Extractiveness is very high (0.95) because this reading aims for complete non-payment, effectively extracting the full value of the reparations from the Allied creditors. Suppression (0.88) is also high, as it requires active political and military measures to resist Allied enforcement attempts and suppress internal dissent. Theater ratio is low (0.1) because the repudiation is a direct, functional rejection of obligations, not a performative maintenance of a defunct system. Resistance is high (0.9) from the Allied side, as they actively sought to enforce the treaty.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of German nationalists, this reading is a necessary act of self-defense and liberation (a 'rope' or even 'mountain' of national sovereignty). From the Allied perspective, it is a clear act of bad faith and aggression (a 'snare'). The engine's classification as 'snare' reflects the structural reality of asymmetric extraction and coercion, regardless of the self-justifying narrative.
 *
 * DIRECTIONALITY LOGIC:
 *   German nationalists and the military-industrial complex are clear beneficiaries, as the repudiation frees resources and fuels their agenda. The German populace is also a beneficiary in terms of immediate economic relief and psychological validation, though they bear long-term risks. Allied creditor nations and the international legal order are the primary victims, suffering direct financial losses and a severe blow to their authority and stability.
 *
 * MANDATROPHY ANALYSIS:
 *   This constraint is not about mandatrophy; it's about the active repudiation of an existing mandate. The 'mandate' of reparations is actively contested and rejected, not merely atrophied. The classification as a snare prevents mislabeling this active extraction as a degraded coordination mechanism.
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_legitimacy_under_duress,
    'Is a treaty imposed under duress (e.g., threat of continued blockade or invasion) legally and morally binding?',
    'Analysis of international legal precedents regarding ''pacta sunt servanda'' versus ''rebus sic stantibus'' and the concept of ''unequal treaties'' in historical context.',
    'If duress is accepted as invalidating, this reading gains legal ground, potentially reclassifying the original treaty as a snare. If duress is deemed insufficient for invalidation, this reading''s claims of illegitimacy are weakened, reinforcing the Allied punitive_liability_reading.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(treaty_legitimacy_under_duress, conceptual, 'The legal and moral validity of treaties signed under perceived coercion.').

omega_variable(
    historical_causality_of_rearmament,
    'To what extent did the repudiation reading of reparations directly enable or accelerate German rearmament, versus other internal and external factors?',
    'Counterfactual historical analysis and econometric modeling of German defense spending and industrial capacity under alternative reparations scenarios.',
    'Strong causal link would reinforce the ''snare'' classification by demonstrating the direct extractive benefit to the German military-industrial complex. Weak link would suggest the repudiation was more symbolic, potentially shifting the classification towards a ''piton'' if the actual material gains were diffuse.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(historical_causality_of_rearmament, empirical, 'The causal link between reparations repudiation and German rearmament.').

omega_variable(
    suppression_mechanism_ambiguity,
    'Is the measured suppression of Allied claims structural (e.g., German military buildup) or internalized (e.g., Allied war-weariness leading to reduced will to enforce)?',
    'Post-repudiation enforcement trajectory: if Allied enforcement efforts persist despite German resistance, suppression is structural. If enforcement wanes due to internal political factors in Allied nations, reclassify as partially internalized.',
    'If internalized, the constraint''s effective suppression is higher than the structural measure suggests — Allied nations carry the suppression with them after initial attempts at enforcement. If structural, the high suppression is a direct result of German actions.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(suppression_mechanism_ambiguity, empirical, 'Structural vs. internalized suppression mechanism for Allied claims.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__repudiation_reading, 1919, 1939).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1919, 0.2).
narrative_ontology:measurement(vers_tr_t1925, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1925, 0.15).
narrative_ontology:measurement(vers_tr_t1930, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1930, 0.12).
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
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, locarno_treaties).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, dawes_plan).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, young_plan).

% DUAL FORMULATION NOTE:
% This is one of three readings of the Versailles Reparations Clauses kernel. This 'repudiation reading' directly challenges the legitimacy and enforceability of the clauses, influencing the operational context of the 'punitive liability' and 'limited responsibility' readings.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
