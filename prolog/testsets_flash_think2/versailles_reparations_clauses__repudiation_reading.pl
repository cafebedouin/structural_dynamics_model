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
    narrative_ontology:epsilon_provenance/5,
    narrative_ontology:human_readable/2,
    narrative_ontology:topic_domain/2.

/* ==========================================================================
   1. NARRATIVE CONTEXT
   ========================================================================== */

/**
 * CONSTRAINT IDENTIFICATION
 *   constraint_id: versailles_reparations_clauses__repudiation_reading
 *   human_readable: Repudiation of Versailles Reparations (Repudiation Reading)
 *   domain: international_relations/legal_history/political_economy
 *
 * SUMMARY:
 *   This constraint story instantiates the 'repudiation reading' of the
 *   Versailles Reparations Clauses. It asserts that the Treaty of Versailles
 *   was imposed under duress, rendering its reparations clauses illegitimate
 *   and non-binding on Germany beyond token gestures. This reading actively
 *   seeks to nullify Allied claims, maximize German rearmament space, and
 *   invalidate Allied security guarantees. The high extractiveness reflects
 *   the complete suppression of creditor claims from this reading's
 *   perspective.
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
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, accessibility_collapse, 0.6).
narrative_ontology:constraint_metric(versailles_reparations_clauses__repudiation_reading, resistance, 0.9).

% --- Constraint claim ---
narrative_ontology:constraint_claim(versailles_reparations_clauses__repudiation_reading, snare).
narrative_ontology:human_readable(versailles_reparations_clauses__repudiation_reading, "Repudiation of Versailles Reparations (Repudiation Reading)").
narrative_ontology:topic_domain(versailles_reparations_clauses__repudiation_reading, "international_relations/legal_history/political_economy").

domain_priors:requires_active_enforcement(versailles_reparations_clauses__repudiation_reading).

% --- Commitment system structure ---
narrative_ontology:cs_story_uid(versailles_reparations_clauses__repudiation_reading, '718e2b66-904d-40da-b5c6-fd40b3566428').
narrative_ontology:cs_kernel_codification('718e2b66-904d-40da-b5c6-fd40b3566428', fixed_text).
narrative_ontology:cs_authority_grounding('718e2b66-904d-40da-b5c6-fd40b3566428', extraction).
narrative_ontology:cs_interpretation_layer_present('718e2b66-904d-40da-b5c6-fd40b3566428').
narrative_ontology:cs_reading_relation('718e2b66-904d-40da-b5c6-fd40b3566428', versailles_reparations_clauses__punitive_liability_reading, forecloses).
narrative_ontology:cs_reading_relation('718e2b66-904d-40da-b5c6-fd40b3566428', versailles_reparations_clauses__limited_responsibility_reading, forecloses).
narrative_ontology:cs_axiom('718e2b66-904d-40da-b5c6-fd40b3566428', foundational, treaty_of_versailles_illegitimate_due_to_duress).
narrative_ontology:cs_axiom_status(treaty_of_versailles_illegitimate_due_to_duress, holdable).
narrative_ontology:cs_axiom_grounding('718e2b66-904d-40da-b5c6-fd40b3566428', treaty_of_versailles_illegitimate_due_to_duress, conventional).
narrative_ontology:cs_axiom('718e2b66-904d-40da-b5c6-fd40b3566428', foundational, no_binding_obligation_beyond_token_gestures).
narrative_ontology:cs_axiom_status(no_binding_obligation_beyond_token_gestures, holdable).
narrative_ontology:cs_axiom_grounding('718e2b66-904d-40da-b5c6-fd40b3566428', no_binding_obligation_beyond_token_gestures, deontological).
narrative_ontology:cs_reference_frame('718e2b66-904d-40da-b5c6-fd40b3566428', german_sovereignty_unimpaired).
narrative_ontology:cs_drift_state('718e2b66-904d-40da-b5c6-fd40b3566428', post_treaty_signing, gap(repudiation_pressure, severe, false)).
narrative_ontology:cs_created_at('718e2b66-904d-40da-b5c6-fd40b3566428', '').
narrative_ontology:cs_kernel_id(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses).

% --- Structural relationships ---
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, post_ww1_germany).
narrative_ontology:constraint_beneficiary(versailles_reparations_clauses__repudiation_reading, german_nationalists).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, allied_creditor_nations).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, french_government).
narrative_ontology:constraint_victim(versailles_reparations_clauses__repudiation_reading, belgian_government).

/* ==========================================================================
   2b. STAKEHOLDER LAYER (OQ-83; roles from the DECLARED dial-set —
   see schemas/constraint_story_schema.json $defs/StakeholderRole and the
   attached residue ledger. Names are per-story and domain-specific; never
   standardized across readings (OQ-84).
   ========================================================================== */

% The German state, seeking to escape the punitive reparations and rebuild its economy. It actively promotes the narrative of duress and illegitimacy to justify non-payment and rearmament.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, post_ww1_germany, agenda_setter,
    institutional, generational, constrained, national).

% Political and social movements within Germany that vehemently oppose the Treaty of Versailles, viewing it as a national humiliation and an unjust imposition. They benefit from the repudiation narrative by gaining political capital and popular support.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, german_nationalists, beneficiary,
    organized, biographical, identity_locked, national).

% The collective of nations (primarily France, Belgium, Great Britain) that were owed reparations under the Treaty of Versailles. From the repudiation reading's perspective, they bear the cost of denied claims and lost compensation.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, allied_creditor_nations, payer,
    institutional, generational, constrained, global).

% A primary advocate for strict enforcement of reparations, viewing them as essential for its post-war reconstruction and security against future German aggression. It directly suffers from Germany's repudiation.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, french_government, payer,
    institutional, generational, constrained, national).

% Another key recipient of reparations, having suffered significant damage during the war. Its financial recovery is directly impacted by Germany's refusal to pay.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, belgian_government, payer,
    institutional, generational, constrained, national).

% The international organization established to promote peace and cooperation, tasked with upholding international law and mediating disputes. Its authority is challenged by the repudiation, highlighting its limited enforcement power.
narrative_ontology:constraint_stakeholder(versailles_reparations_clauses__repudiation_reading, league_of_nations, observer,
    institutional, generational, analytical, global).

% --- Six-questions battery (story-level; texts kept as comments — the
% engine consumes only the two atoms below; the founding-problem narrative
% is NEVER consumed as a claim, mismatch-consumer only, OQ-83 R5) ---
% COORDINATION_FUNCTION: Coordinates German national sentiment and political action around a narrative of victimhood and resistance to external imposition, aiming to unify the nation against the treaty's terms.
% TRANSFER_FUNCTION: Aims to prevent the transfer of substantial wealth (reparations payments) from Germany to the Allied creditor nations, effectively transferring the burden of war costs back to the creditors.
% ABSENT_VOICES: The voices of those who suffered directly from German aggression during WWI, particularly civilians and communities in occupied territories, whose claims for compensation are denied by this repudiation. Also, those who advocated for a more conciliatory approach to Germany but were sidelined by punitive demands.
% DISAPPEARANCE_RATIONALE: If the repudiation reading vanished, Germany would likely resume some form of reparations payment, altering its economic and political trajectory, potentially leading to different international relations and financial burdens for its economy. The post-WWI international order would have been fundamentally different.
% FOUNDING_PROBLEM: The perceived injustice, economic burden, and national humiliation imposed on Germany by the Treaty of Versailles, particularly Article 231 (the 'War Guilt Clause') and the subsequent reparations demands.
% FOUNDING_PROBLEM_CORROBORATION: German historians, economists, and political figures of the era widely corroborated the economic burden and perceived injustice. While the 'duress' argument was contested by Allied powers, the internal German experience of the treaty as an unbearable imposition was well-documented by sources outside the immediate benefiting parties (e.g., some neutral observers, later historical analyses).
narrative_ontology:disappearance_verdict(versailles_reparations_clauses__repudiation_reading, world_rearranges).
narrative_ontology:founding_problem_status(versailles_reparations_clauses__repudiation_reading, live).

/* ==========================================================================
   3. PROVENANCE (cohort metadata — schema-required since Phase C)
   ========================================================================== */

narrative_ontology:story_provenance(versailles_reparations_clauses__repudiation_reading, '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28',
    '685ed7cf90d7b7bdcefb4b3c4e62d9bf2aa6ee28', '2026-08-21',
    'no_scope_rebuild_gemini_think2', 'agent/example_platform_commission.json',
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

:- end_tests(versailles_reparations_clauses__repudiation_reading_tests).

/* ==========================================================================
   5. GENERATIVE COMMENTARY
   ========================================================================== */

/**
 * LOGIC RATIONALE:
 *   The extractiveness is high (0.90) because this reading aims for total denial of reparations, effectively extracting the value of those claims from the Allied nations. Suppression is also high (0.85) as it requires active political, diplomatic, and eventually military efforts by Germany to resist and nullify the treaty's enforcement. The theater ratio is low (0.10) because this is a direct, confrontational challenge to an existing international legal framework, not a performative maintenance of an atrophied function.
 *
 * PERSPECTIVAL GAP:
 *   From the perspective of the Allied creditor nations, the repudiation is a breach of international law and a denial of legitimate compensation for war damages. From the German perspective (as articulated by this reading), it is a necessary act of national liberation from an unjust and economically ruinous imposition. The engine computes this divergence from the structural data; the authored claim does not adjudicate it.
 *
 * DIRECTIONALITY LOGIC:
 *   Post-WWI Germany and German nationalists are the beneficiaries, as they seek to avoid payment and regain sovereignty, with their directionality near the beneficiary end. Allied creditor nations, particularly France and Belgium, are the targets, as their claims are being denied, placing their directionality near the target end. The League of Nations is an observer, attempting to uphold the treaty but lacking the power to enforce it against a determined repudiation.
 *
 */

/* ==========================================================================
   6. OMEGA VARIABLES (Ω) - IRREDUCIBLE UNCERTAINTIES
   ========================================================================== */

omega_variable(
    treaty_legitimacy_under_duress,
    'Was the Treaty of Versailles truly illegitimate due to duress, or was it a valid outcome of war, albeit a harsh one?',
    'Analysis of international legal precedents regarding treaties signed under duress, and historical evidence of the negotiating conditions and alternatives available to Germany at the time.',
    'If deemed illegitimate, the repudiation reading gains stronger legal and moral grounding, potentially reclassifying the original treaty as a Snare. If deemed legitimate, the repudiation becomes a clear breach of international law, strengthening the Allied claims.',
    confidence_without_resolution(medium)
).

narrative_ontology:omega_variable(treaty_legitimacy_under_duress, conceptual, 'Ambiguity regarding the legal and moral legitimacy of the Treaty of Versailles due to the circumstances of its signing.').

omega_variable(
    reparations_economic_capacity_vs_political_will,
    'To what extent was Germany''s inability to pay reparations a genuine economic constraint, versus a political choice driven by the repudiation narrative?',
    'Independent economic analysis of Germany''s post-WWI industrial capacity, financial resources, and alternative economic policies, compared to the actual reparations schedule and payments.',
    'If economic capacity was genuinely insufficient, it would lend credence to the ''limited_responsibility_reading'' and potentially mitigate the ''snare'' classification of the repudiation. If political will was the primary driver, it reinforces the ''snare'' classification of the repudiation as an active extraction from creditors.',
    confidence_without_resolution(high)
).

narrative_ontology:omega_variable(reparations_economic_capacity_vs_political_will, empirical, 'Distinguishing between economic impossibility and political unwillingness in Germany''s reparations payments.').


/* ==========================================================================
   7. INTEGRATION HOOKS
   ========================================================================== */

narrative_ontology:interval(versailles_reparations_clauses__repudiation_reading, 1919, 1933).

/* ==========================================================================
   8. TEMPORAL MEASUREMENTS (LIFECYCLE DRIFT DATA)
   ========================================================================== */

% Theater ratio over time
narrative_ontology:measurement(vers_tr_t1919, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1919, 0.15).
narrative_ontology:measurement(vers_tr_t1923, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1923, 0.12).
narrative_ontology:measurement(vers_tr_t1926, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1926, 0.1).
narrative_ontology:measurement(vers_tr_t1929, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1929, 0.08).
narrative_ontology:measurement(vers_tr_t1933, versailles_reparations_clauses__repudiation_reading, theater_ratio, 1933, 0.1).

% Extraction over time
narrative_ontology:measurement(vers_be_t1919, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1919, 0.7).
narrative_ontology:measurement(vers_be_t1923, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1923, 0.75).
narrative_ontology:measurement(vers_be_t1926, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1926, 0.8).
narrative_ontology:measurement(vers_be_t1929, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1929, 0.85).
narrative_ontology:measurement(vers_be_t1933, versailles_reparations_clauses__repudiation_reading, base_extractiveness, 1933, 0.9).

% Suppression requirement over time
narrative_ontology:measurement(vers_su_t1919, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1919, 0.65).
narrative_ontology:measurement(vers_su_t1923, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1923, 0.7).
narrative_ontology:measurement(vers_su_t1926, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1926, 0.75).
narrative_ontology:measurement(vers_su_t1929, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1929, 0.8).
narrative_ontology:measurement(vers_su_t1933, versailles_reparations_clauses__repudiation_reading, suppression_requirement, 1933, 0.85).


/* ==========================================================================
   9. BOLTZMANN & NETWORK DATA
   ========================================================================== */

narrative_ontology:coordination_type(versailles_reparations_clauses__repudiation_reading, enforcement_mechanism).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__punitive_liability_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, versailles_reparations_clauses__limited_responsibility_reading).
narrative_ontology:affects_constraint(versailles_reparations_clauses__repudiation_reading, league_of_nations_authority).

% DUAL FORMULATION NOTE:
% This constraint is one of three readings of the 'versailles_reparations_clauses' kernel, each representing a distinct structural claim about the treaty's nature and obligations.

/* ==========================================================================
   10. DIRECTIONALITY OVERRIDES (v6.0, OPTIONAL)
   ========================================================================== */

/* ==========================================================================
   END OF CONSTRAINT STORY
   ========================================================================== */
